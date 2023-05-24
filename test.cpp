#include "tiny_test.hpp"
#include "unordered_map.h"

#include <algorithm>
#include <iterator>
#include <ranges>
#include <compare>
#include <numeric>


using testing::make_test;
using testing::SimpleTest;
using testing::PrettyTest;
using testing::TestGroup;
using groups_t = std::vector<TestGroup>;
using namespace std::views;
namespace rng = std::ranges;


struct Data {
    int data;

    auto operator<=>(const Data&) const = default;
};

struct Trivial : Data {};
constexpr Trivial operator ""_tr(unsigned long long int x) {
    return Trivial{int(x)};
}

struct NonTrivial : Data {
    NonTrivial(){}
    NonTrivial(int x) {
        data = x;
    }
};
NonTrivial operator ""_ntr(unsigned long long int x) {
    return NonTrivial{int(x)};
}

struct NotDefaultConstructible {
    NotDefaultConstructible() = delete;
    NotDefaultConstructible(int input): data(input) {}
    int data;

    auto operator<=>(const NotDefaultConstructible&) const = default;
};

struct NeitherDefaultNorCopyConstructible {
    NeitherDefaultNorCopyConstructible() = delete;
    NeitherDefaultNorCopyConstructible(const NeitherDefaultNorCopyConstructible&) = delete;
    NeitherDefaultNorCopyConstructible(NeitherDefaultNorCopyConstructible&& other)
    : data(other.data)
    , moved(true){ };
    NeitherDefaultNorCopyConstructible(int input)
    : data(input)
    , moved(false) {}
    int data;
    const bool moved = false;

    auto operator<=>(const NeitherDefaultNorCopyConstructible&) const = default;
};

template<typename T>
struct NotPropagatedCountingAllocator {
    size_t allocates_counter = 0;
    size_t deallocates_counter = 0;

    using propagate_on_container_move_assignment = std::false_type;
    using value_type = T;

    NotPropagatedCountingAllocator() = default;

    template<typename U>
    NotPropagatedCountingAllocator(const NotPropagatedCountingAllocator<U>& other)
    : allocates_counter(other.allocates_counter)
    , deallocates_counter(other.deallocates_counter) {}

    template<typename U>
    NotPropagatedCountingAllocator(NotPropagatedCountingAllocator<U>&& other)
    : allocates_counter(other.allocates_counter)
    , deallocates_counter(other.deallocates_counter) {
        other.allocates_counter = 0;
        other.deallocates_counter = 0;
    }

    bool operator==(const NotPropagatedCountingAllocator<T>&) const {
        return false;
    }

    T* allocate(size_t n) {
        ++allocates_counter;
        return std::allocator<T>().allocate(n);
    }

    void deallocate(T* pointer, size_t n) {
        ++deallocates_counter;
        return std::allocator<T>().deallocate(pointer, n);
    }
};

const size_t small_size = 17;
const size_t medium_size = 100;

constexpr size_t operator ""_sz(unsigned long long int x) {
    return size_t(x);
}

template<typename Value>
auto make_small_map(){
    UnorderedMap<int, Value> map;
    for (int i = 0; i < int(small_size); ++i) {
        map.emplace(i, Value{i});
    }
    return map;
}

bool maps_equal(const auto& left, const auto& right) {
    return rng::all_of(left, [&right](const auto& pr){
        return right.at(pr.first) == pr.second;
    });
}

//NOLINTNEXTLINE
TestGroup create_constructor_tests() {
    return { "construction and assignment",
        make_test<PrettyTest>("default", [](auto& test){
            UnorderedMap<int, int> defaulted;
            test.equals(defaulted.size(), 0_sz);
            UnorderedMap<int, NotDefaultConstructible> without_default;
            test.equals(without_default.size(), 0_sz);
        }),

        make_test<PrettyTest>("copy and move", [&](auto& test){
            {
                auto map = make_small_map<Trivial>();
                auto copy = map;
                test.check(maps_equal(copy, map));
                auto move_copy = std::move(map);
                test.check(maps_equal(copy, move_copy));
                test.equals(map.size(), 0_sz);
            }
            {
                auto map = make_small_map<NonTrivial>();
                auto copy = map;
                test.check(maps_equal(copy, map));
                auto move_copy = std::move(map);
                test.check(maps_equal(copy, move_copy));
                test.equals(map.size(), 0_sz);
            }
        }),

        make_test<PrettyTest>("assignment operators", [](auto& test){
            auto map = make_small_map<Trivial>();
            test.equals(map.size(), small_size);
            UnorderedMap<int, Trivial> map2;
            test.equals(map2.size(), 0_sz);

            map2 = map;
            test.check(maps_equal(map, map2));
            map2 = std::move(map);
            test.equals(map.size(), 0_sz);
            test.equals(map2.size(), small_size);
        }),

        make_test<PrettyTest>("move assignment with unequal and not propagating allocator", [](auto& test){
            using custom_alloc_type = NotPropagatedCountingAllocator<std::pair<const int, NeitherDefaultNorCopyConstructible>>;
            using special_map_type = UnorderedMap<int, NeitherDefaultNorCopyConstructible, 
                    std::hash<int>, std::equal_to<int>, 
                    custom_alloc_type>;
            special_map_type map;
            map.emplace(1, NeitherDefaultNorCopyConstructible(1));
            auto address1 = std::addressof(*map.begin());
            special_map_type map_moved;
            test.check(map_moved.get_allocator() != map.get_allocator());
            map_moved = std::move(map);
            auto address2 = std::addressof(*map_moved.begin());
            test.check(map_moved.at(1).moved);
            test.check(address1 != address2);
        }),

        make_test<PrettyTest>("swap", [](auto& test){
            auto map = make_small_map<Trivial>();      
            decltype(map) another;
            auto it = map.find(1);   
            auto address = &(*it);
            test.equals(it->second, 1_tr);
            map.swap(another);
            test.equals(it->second, 1_tr);
            test.equals(address->second, 1_tr);
        })

    };
}

//NOLINTNEXTLINE
TestGroup create_modification_tests() {
    return { "modification",
        make_test<PrettyTest>("emplace", [](auto& test){
            UnorderedMap<int, NonTrivial> map;
            auto [place, did_insert] = map.emplace(1, 1_ntr);
            test.equals(map.at(1), 1_ntr);
            test.equals(place, map.begin());
            test.check(did_insert);
            auto [new_place, new_did_insert] = map.emplace(2, 2_ntr);
            // update place as it could be invalidated by rehash
            place = map.find(1);
            test.check(place != new_place);
            test.check(new_did_insert);
            test.equals(map.at(2), 2_ntr);
            test.equals(map.at(1), 1_ntr);
            auto [old_place, reinsert] = map.emplace(1, 3_ntr);
            test.check(!reinsert);
            test.equals(old_place, place);
            test.equals(map.at(1), 1_ntr);
            test.equals(map.at(2), 2_ntr);
        }),

        make_test<PrettyTest>("emplace move", [](auto& test){
            UnorderedMap<std::string, std::string> moving_map;
            std::string a = "a";
            std::string b = "b";
            std::string c = "c";
            moving_map.emplace(a, a);
            test.equals(a, "a");
            moving_map.emplace(std::move(b), a);
            test.equals(a, "a");
            test.equals(b, "");
            moving_map.emplace(std::move(c), std::move(a));
            test.equals(a, "");
            test.equals(c, "");
            test.equals(moving_map.size(), 3_sz);
            test.equals(moving_map.at("a"), "a");
            test.equals(moving_map.at("b"), "a");
            test.equals(moving_map.at("c"), "a");
        }),

        make_test<PrettyTest>("insert nontrivial", [](auto& test){
            UnorderedMap<int, NonTrivial> map;
            auto [place, did_insert] = map.insert({1, 1_ntr});
            test.equals(map.at(1), 1_ntr);
            test.equals(place, map.begin());
            test.check(did_insert);
            auto [new_place, new_did_insert] = map.insert({2, 2_ntr});
            place = map.find(1);
            test.check(place != new_place);
            test.check(new_did_insert);
            test.equals(map.at(2), 2_ntr);
            test.equals(map.at(1), 1_ntr);
            auto [old_place, reinsert] = map.insert({1, 3_ntr});
            test.check(!reinsert);
            test.equals(old_place, place);
            test.equals(map.at(1), 1_ntr);
            test.equals(map.at(2), 2_ntr);
        }),

        make_test<PrettyTest>("insert move", [](auto& test){
            UnorderedMap<std::string, std::string> moving_map;
            using node = std::pair<std::string, std::string>;
            
            node a{"a", "a"};
            node b{"b", "b"};
            
            moving_map.insert(a);
            test.equals("a", a.first);
            test.equals(moving_map.size(), 1_sz);

            moving_map.insert(std::move(b));
            test.equals("", b.first);
            test.equals(moving_map.size(), 2_sz);
            
            test.equals(moving_map.at("a"), "a");
            test.equals(moving_map.at("b"), "b");
        }),

        make_test<PrettyTest>("insert range", [](auto& test) {
            UnorderedMap<int, NonTrivial> map;
            /*auto range = iota(0, int(medium_size))
                | transform([](int item) -> std::pair<const int, NonTrivial> { return {item, {item}}; } )
                | common;*/
            std::vector<std::pair<int, NonTrivial>> range;
            for (int i = 0; i < int(medium_size); ++i) {
                range.emplace_back(i, NonTrivial{i});
            }
            map.insert(range.begin(), range.end());
            /*test.check(rng::all_of(iota(0, int(medium_size)), [&](int item) { 
                return map.at(item) == NonTrivial{item};
            }));*/
            std::vector<int> indices(small_size);
            std::iota(indices.begin(), indices.end(), 0);
            test.check(std::all_of(indices.begin(), indices.end(), [&](int item) {
                return map.at(item) == NonTrivial{item};           
            }));
        }),

        make_test<PrettyTest>("move insert range", [](auto& test) {
            UnorderedMap<int, std::string> map;
            std::vector<int> indices(small_size);
            std::iota(indices.begin(), indices.end(), 0);
            /*auto range = iota(0, int(small_size))
                | transform([](int item) -> std::pair<const int, std::string> {
                        return {item, std::to_string(item)}; 
                })
                | common;*/
            std::vector<std::pair<int, std::string>> storage;
            std::transform(indices.begin(), indices.end(), std::back_inserter(storage), [](int idx){
                return std::pair<const int, std::string>{idx, std::to_string(idx)};        
            });
            map.insert(storage.begin(), storage.end());
            test.check(rng::all_of(storage, [](auto& pr) { 
                return std::to_string(pr.first) == pr.second;
            }));
            map = UnorderedMap<int, std::string>();
            map.insert(std::move_iterator(storage.begin()), std::move_iterator(storage.end()));
            test.equals(storage.size(), small_size);
            test.check(rng::all_of(storage, [&](auto& p) {
                return test.equals(p.second, "");
            }));
        })
    };
}

TestGroup create_access_tests() {
    return { "access",
        make_test<PrettyTest>(".at and []", [](auto& test){
            /*auto range = iota(0, int(small_size))
                | transform([](int item) -> std::pair<const int, std::string> {
                        return {item, std::to_string(item)}; 
                })
                | common;*/
            std::vector<int> indices(small_size);
            std::iota(indices.begin(), indices.end(), 0);
            std::vector<std::pair<int, std::string>> range;
            std::transform(indices.begin(), indices.end(), std::back_inserter(range), [](int idx){
                return std::pair<const int, std::string>{idx, std::to_string(idx)};        
            });

            std::iota(indices.begin(), indices.end(), 0);
            UnorderedMap<int, std::string> map;
            map.insert(range.begin(), range.end());
            for (int idx : indices) {
                test.equals(std::to_string(idx), map.at(idx));
                test.equals(std::to_string(idx), map[idx]);
            }
            try {
                map.at(-1) = "abacaba";
                test.fail();
            } catch(...) { }
            map[-1] = "abacaba";
            test.equals(map.at(-1), "abacaba");
            map.at(-1) = "qwerty";
            test.equals(map[-1], "qwerty");
        }),

        make_test<PrettyTest>("[] move", [](auto& test){
            UnorderedMap<std::string, std::string> map;
            /*auto range = iota(0, int(small_size))
                | transform([](int item) {
                        return std::to_string(item); 
                })
                | common;*/
            std::vector<int> indices(small_size);
            std::iota(indices.begin(), indices.end(), 0);
            std::vector<std::string> storage;
            std::transform(indices.begin(), indices.end(), std::back_inserter(storage), [](int idx){
                return std::to_string(idx);       
            });
            test.check(rng::all_of(indices, [&](int i) {
                return std::to_string(i) == storage[size_t(i)];    
            }));
            map[std::move(storage[0])] = std::move(storage[1]);
            test.equals(storage[0], "");
            test.equals(storage[1], "");
            map[std::move(storage[2])] = storage[3];
            test.equals(storage[2], "");
            test.equals(storage[3], "3");
            map[storage[3]] = std::move(storage[4]);
            test.equals(storage[3], "3");
            test.equals(storage[4], "");
        }),

        make_test<PrettyTest>("find", [](auto& test){
            auto map = make_small_map<Trivial>();
            auto existing = map.find(1);
            test.equals(existing->second, 1_tr);
            auto non_existing = map.find(-1);
            test.equals(non_existing, map.end());
        }),

        make_test<PrettyTest>("bucket borders check", [](auto& test){
            UnorderedMap<int, int, decltype([](int element) -> size_t {
                return static_cast<size_t>(abs(element) % 10);
            })> map;
            map.emplace(1, 1);
            map.emplace(11, 11);
            test.check(map.find(1) != map.end());
            test.check(map.find(11) != map.end());
            test.check(map.find(11) != map.find(1));
        })
    };
}

TestGroup create_misc_tests() {
    return { "misc",
        make_test<PrettyTest>("load factor", [](auto& test) {
            auto map = make_small_map<Trivial>();
            //auto max_val = rng::max(map | keys);
            auto max_val = std::max_element(map.begin(), map.end(), [](auto& left, auto& right) { return left.first < right.first; })->first;
            test.check(map.load_factor() > 0.0f);
            auto new_load_factor = map.load_factor() / 2.0f;
            map.max_load_factor(new_load_factor);
            //for (auto i : iota(max_val + 1) | take(medium_size)) {
            for (auto i = max_val + 1; i < max_val + 1 + int(medium_size); ++i) {
                auto [_, inserted] = map.emplace(i, Trivial{i});
                test.check(inserted);
                test.check(map.load_factor() > 0.0f);
                test.check(map.load_factor() <= new_load_factor);
            }
        })
    };
}


int main() {
    groups_t groups {};
    groups.push_back(create_constructor_tests());
    groups.push_back(create_modification_tests());
    groups.push_back(create_access_tests());
    groups.push_back(create_misc_tests());

    bool res = true;
    for (auto& group : groups) {
        res &= group.run();
    }

    return res ? 0 : 1;
}

