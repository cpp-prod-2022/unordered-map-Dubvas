#pragma once
#include <memory>
#include <utility>
#include <cassert>
#include <stdexcept>
#include <optional>
#include <vector>
#include <cmath>
#include <unordered_map>


template<typename Key, typename Value, class Hash = std::hash<Key>, class Equal = std::equal_to<Key>, class Alloc = std::allocator<std::pair<const Key, Value>>>
class UnorderedMap {
    template<typename T, class Allocator = std::allocator<T>>
    class List {
    private:
        friend UnorderedMap;
        struct BaseNode {
            BaseNode* prev;
            BaseNode* next;
            BaseNode() = default;
            BaseNode(bool self) {
                if (self)
                    next = prev = this;
            }
            BaseNode(BaseNode* prev, BaseNode* next) : prev(prev), next(next) {}
            BaseNode(const BaseNode& that) = delete;
            BaseNode(BaseNode&& that) : prev(that.prev), next(that.next) {
                if (that.next == &that) {
                    next = this;
                    prev = this;
                }
                else {
                    next->prev = this;
                    prev->next = this;
                }
                that.next = that.prev = &that;
            }
            ~BaseNode() {}

            BaseNode& operator=(const BaseNode& that) = delete;
            BaseNode& operator=(BaseNode&& that) {
                assert(next == this);
                next = that.next;
                prev = that.prev;
                if (that.next == &that) {
                    next = this;
                    prev = this;
                }
                else {
                    next->prev = this;
                    prev->next = this;
                }
                that.next = that.prev = &that;
                return *this;
            }

            void swap(BaseNode& that) {
                BaseNode tmp(std::move(that));
                that = std::move(*this);
                *this = std::move(tmp);
            }
        };

        class Node : public BaseNode {
        public:
            T value;
            Node() = default;
            Node(BaseNode* prev, BaseNode* next) : BaseNode(prev, next), value() {}
            Node(const T& value, BaseNode* prev, BaseNode* next) : BaseNode(prev, next), value(value) {}
            Node(T&& value, BaseNode* prev, BaseNode* next) : BaseNode(prev, next), value(std::move(value)) {}
        };

        template<typename U>
        class BaseIterator { // NOLINT
        private:
            friend List;
            friend UnorderedMap;
            BaseNode* ptr;
        public:
            using value_type = U;
            using pointer = U*;
            using reference = U&;
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::ptrdiff_t; // it just needs this
            BaseIterator(BaseNode* ptr) : ptr(ptr) {}
        public:
            BaseIterator() {}
            BaseIterator(const BaseIterator& that) = default;
            ~BaseIterator() = default;

            operator BaseIterator<const T>() const {
                return BaseIterator<const T>(ptr);
            }

            BaseIterator& operator=(const BaseIterator& that) = default;

            BaseIterator& operator++() {
                ptr = ptr->next;
                return *this;
            }
            BaseIterator& operator--() {
                ptr = ptr->prev;
                return *this;
            }
            BaseIterator operator++(int) {
                BaseIterator tmp(*this);
                ++(*this);
                return tmp;
            }
            BaseIterator operator--(int) {
                BaseIterator tmp(*this);
                --(*this);
                return tmp;
            }

            bool operator==(const BaseIterator& that) const = default;

            reference operator*() const {
                return static_cast<reference>(static_cast<Node*>(ptr)->value);
            }
            pointer operator->() const {
                return &static_cast<Node*>(ptr)->value;
            }
        };
    public:
        using value_type = T;
        using iterator = BaseIterator<T>;
        using const_iterator = BaseIterator<const T>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    private:
        using NodeAllocType = typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;
        using AllocTraits = std::allocator_traits<NodeAllocType>;
        [[no_unique_address]] NodeAllocType node_alloc;
        BaseNode root;
        size_t sz = 0;

        void clear() noexcept {
            while (sz)
                pop_back();
        }

    public:
        ~List() {
            clear();
        }

        List() : List(Allocator()) {}
        List(size_t size) : List(size, Allocator()) {}
        List(size_t size, const T& value) : List(size, value, Allocator()) {}

        List(const Allocator& alloc) : node_alloc(alloc), root(true), sz(0) {}
        List(size_t size, const Allocator& alloc) : List(size, T(), alloc) {}
        List(size_t size, const T& value, const Allocator& alloc) : node_alloc(alloc), root(true), sz(0) {
            for (size_t i = 0; i < size; ++i) {
                try {
                    push_back(value);
                }
                catch (...) {
                    clear();
                    throw;
                }
            }
        }

        List(const List& that) : node_alloc(std::allocator_traits<Allocator>::select_on_container_copy_construction(that.node_alloc)), root(true) {
            try {
                for (const auto& value : that) {
                    push_back(value);
                }
            }
            catch (...) {
                clear();
                throw;
            }
        }


        List(List&& that) : node_alloc(std::move(that.node_alloc)), root(std::move(that.root)), sz(that.sz) {
            that.sz = 0;
        }

        List& operator=(const List& that) {
            List tmp(get_allocator());
            if constexpr (AllocTraits::propagate_on_container_copy_assignment::value)
                tmp.node_alloc = that.node_alloc;
            try {
                for (const auto& value : that) {
                    tmp.push_back(value);
                }
            }
            catch (...) {
                tmp.clear();
                throw;
            }
            tmp.root.swap(root);
            std::swap(tmp.sz, sz);
            std::swap(tmp.node_alloc, node_alloc);
            return *this;
        }

        List& operator=(List&& that) {
            clear();
            if constexpr (AllocTraits::propagate_on_container_copy_assignment::value)
                node_alloc = std::move(that.node_alloc);
            sz = that.sz;
            that.sz = 0;
            root = std::move(that.root);
            return *this;
        }

        void push_back(const T& value) {
            insert(const_iterator(&root), value);
        }
        void push_front(const T& value) {
            insert(const_iterator(root.next), value);
        }

        void pop_back() noexcept {
            erase(root.prev);
        }
        void pop_front() noexcept {
            erase(root.next);
        }

        iterator insert(const_iterator pos, const T& value) {
            T tmp(value);
            return insert(pos, std::move(tmp));
        }

        iterator insert(const_iterator pos, T&& value) {
            Node* element = AllocTraits::allocate(node_alloc, 1);
            try {
                AllocTraits::construct(node_alloc, element, std::move(value), pos.ptr->prev, pos.ptr);
                pos.ptr->prev->next = element;
                pos.ptr->prev = element;
                ++sz;
            }
            catch (...) {
                AllocTraits::deallocate(node_alloc, element, 1);
                throw;
            }
            return iterator(static_cast<BaseNode*>(element));
        }

        Node* extractNode(const_iterator it) {
            BaseNode* prev = it.ptr->prev;
            BaseNode* next = it.ptr->next;
            prev->next = next;
            next->prev = prev;
            it.ptr->prev = it.ptr->next = nullptr;
            --sz;
            return static_cast<Node*>(it.ptr);
        }

        iterator insertNode(const_iterator it, Node* node) {
            BaseNode* next = it.ptr;
            BaseNode* prev = next->prev;
            node->next = next;
            node->prev = prev;
            next->prev = node;
            prev->next = node;
            ++sz;
            return iterator(node);
        }

        iterator erase(const_iterator pos) noexcept {
            Node* element = static_cast<Node*>(pos.ptr);
            element->next->prev = element->prev;
            element->prev->next = element->next;
            auto result = iterator(element->next);
            AllocTraits::destroy(node_alloc, element);
            AllocTraits::deallocate(node_alloc, element, 1);
            --sz;
            return result;
        }

        size_t size() const {
            return sz;
        }

        const Allocator get_allocator() const {
            return static_cast<Allocator>(node_alloc);
        }

        auto begin() {
            return iterator(root.next);
        }
        auto end() {
            return iterator(&root);
        }
        auto begin() const {
            return cbegin();
        }
        auto end() const {
            return cend();
        }
        auto cbegin() const {
            return const_iterator(root.next);
        }
        auto cend() const {
            return const_iterator(root.next->prev);
        }

        auto rbegin() {
            return reverse_iterator(end());
        }
        auto rend() {
            return reverse_iterator(begin());
        }
        auto rbegin() const {
            return crbegin();
        }
        auto rend() const {
            return crend();
        }
        auto crbegin() const {
            return const_reverse_iterator(cend());
        }
        auto crend() const {
            return const_reverse_iterator(cbegin());
        }
    };
public:
    using NodeType = std::pair<const Key, Value>;
private:
    struct ListElement {
        NodeType value;
        size_t hash;
        ListElement(NodeType&& value, size_t hash) : value(NodeType{ std::move(const_cast<Key&>(value.first)), std::move(value.second) }), hash(hash) {};
        ListElement(ListElement&& that) : value(NodeType{ std::move(const_cast<Key&>(that.value.first)), std::move(that.value.second) }), hash(std::move(that.hash)) {};
        ListElement(const ListElement& that) = default;
        ListElement& operator=(ListElement&& that) {
            value = NodeType{ std::move(const_cast<Key&>(that.value.first)), std::move(that.value.second) };
            hash = std::move(that.hash);
            return *this;
        }
        ListElement& operator=(const ListElement& that) = default;
        ~ListElement() = default;
    };

    static constexpr size_t base_buckets = 4;
    using AllocTraits = std::allocator_traits<Alloc>;
    using ListType = List<ListElement, Alloc>;
    using LIterator = typename ListType::iterator;

    template<typename T>
    class BaseIterator {
        friend UnorderedMap;
    private:
        LIterator it;
    public:
        using value_type = T;
        using pointer = T*;
        using reference = T&;
        using iterator_category = std::bidirectional_iterator_tag;
        using difference_type = std::ptrdiff_t; // it just needs this
        BaseIterator(LIterator it) : it(it) {}
    public:
        BaseIterator() {}
        BaseIterator(const BaseIterator& that) noexcept = default;
        ~BaseIterator() = default;

        operator BaseIterator<const T>() const noexcept {
            return BaseIterator<const T>(it);
        }

        BaseIterator& operator=(const BaseIterator& that) noexcept = default;
        BaseIterator& operator++() {
            ++it;
            return *this;
        }
        BaseIterator& operator--() {
            --it;
            return *this;
        }
        BaseIterator operator++(int) {
            BaseIterator tmp(*this);
            ++(*this);
            return tmp;
        }
        BaseIterator operator--(int) {
            BaseIterator tmp(*this);
            --(*this);
            return tmp;
        }

        bool operator==(const BaseIterator& that) const = default;
        bool operator!=(const BaseIterator& that) const = default;

        reference operator*() const {
            return static_cast<reference>(it->value);
        }
        pointer operator->() const {
            return &it->value;
        }
    };

    [[no_unique_address]] Alloc allocator;
    [[no_unique_address]] Hash hasher;
    [[no_unique_address]] Equal equer;
    std::vector<std::optional<LIterator>, typename AllocTraits::template rebind_alloc<std::optional<LIterator>>> table;
    ListType bucket;
    float max_lf = 1.0;

    std::pair<LIterator, bool> myFind(const Key& key) const {
        size_t hash = hasher(key);
        auto start_o = table[hash % table.size()];
        if (!start_o)
            return std::pair<LIterator, bool>{ end().it, false };
        auto start = *start_o;
        while (start != bucket.end() && start->hash % table.size() == hash % table.size()) {
            if (equer(start->value.first, key))
                return std::pair<LIterator, bool>{ start, true };
            ++start;
        }
        return std::pair<LIterator, bool>{ start, false };
    }

    LIterator inPlaceInsert(LIterator it, NodeType&& ins) {
        size_t hash = hasher(ins.first);
        it = bucket.insert(it, ListElement(std::move(ins), hash));
        if (!table[hash % table.size()])
            table[hash % table.size()] = it;
        if (load_factor() > max_load_factor())
            rehash(static_cast<size_t>(static_cast<float>(2 * table.size() + 1) / max_load_factor()) + 1);
        return it;
    }

    template<typename... Args>
    NodeType* correctConstruct(Args&&... args) {
        NodeType* tmp = AllocTraits::allocate(allocator, 1);
        try {
            AllocTraits::construct(allocator, tmp, std::forward<Args>(args)...);
        }
        catch (...) {
            AllocTraits::deallocate(allocator, tmp, 1);
            throw;
        }
        return tmp;
    }

public:
    using iterator = BaseIterator<NodeType>;
    using const_iterator = BaseIterator<const NodeType>;
public:

    void rehash(size_t count = 0) {
        size_t min_required = static_cast<size_t>(std::ceil(static_cast<float>(size()) / max_load_factor()));
        if (count < min_required)
            count = min_required;
        table.assign(count, std::nullopt);
        ListType new_bucket(bucket.get_allocator());
        LIterator curr = bucket.begin();
        LIterator next;
        while (curr != bucket.end()) {
            next = curr;
            ++next;
            typename ListType::Node* curr_node = bucket.extractNode(curr);
            LIterator put = new_bucket.end();
            if (table[curr_node->value.hash % table.size()])
                put = *table[curr_node->value.hash % table.size()];
            put = new_bucket.insertNode(put, curr_node);
            table[curr_node->value.hash % table.size()] = put;
            curr = next;
        }
        new_bucket.root.swap(bucket.root);
        std::swap(new_bucket.sz, bucket.sz);
    }

    void reserve(size_t count) {
        rehash(std::ceil(static_cast<float>(count) / max_load_factor()));
    }

    UnorderedMap() : UnorderedMap(base_buckets) {}
    explicit UnorderedMap(size_t bucket_count) : UnorderedMap(bucket_count, Alloc()) {}
    UnorderedMap(size_t bucket_count, const Alloc& alloc) : UnorderedMap(bucket_count, Hash(), alloc) {}
    UnorderedMap(size_t bucket_count, const Hash& hash, const Alloc& alloc) : allocator(alloc), hasher(hash), equer(), table(bucket_count, allocator), bucket(allocator) {}
    explicit UnorderedMap(const Alloc& alloc) : UnorderedMap(base_buckets, alloc) {};

    UnorderedMap(UnorderedMap&& that) noexcept = default;
    UnorderedMap(const UnorderedMap& that) : allocator(AllocTraits::select_on_container_copy_construction(that.allocator)),
        hasher(that.hasher), equer(that.equer), table(base_buckets, allocator), bucket(allocator) {
        for (const auto& node : that) {
            insert(node);
        }
    }

    ~UnorderedMap() {}

    UnorderedMap& operator=(const UnorderedMap& that) {
        if (AllocTraits::propagate_on_container_copy_assignment::value)
            allocator = that.allocator;
        hasher = that.hasher;
        equer = that.equer;
        table = that.table;
        bucket = that.bucket;
        max_lf = that.max_lf;
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& that) {
        hasher = std::move(that.hasher);
        equer = std::move(that.equer);
        max_lf = std::move(that.max_lf);
        if (AllocTraits::propagate_on_container_move_assignment::value) {
            allocator = std::move(that.allocator);
            table = std::move(that.table);
            bucket.clear();
            bucket = std::move(that.bucket);
        }
        else {
            table.assign(that.table.size(), std::nullopt);
            bucket.clear();
            for (auto&& node : that)
                insert(std::move(node));
        }
        return *this;
    }

    void swap(UnorderedMap& that) {
        UnorderedMap tmp = std::move(that);
        that = std::move(*this);
        *this = std::move(tmp);
    }

    Value& at(const Key& key) {
        auto [it, res] = myFind(key);
        if (!res)
            throw std::out_of_range("You have fucked up");
        return it->value.second;
    }

    const Value& at(const Key& key) const {
        auto [it, res] = myFind(key);
        if (!res)
            throw std::out_of_range("You have fucked up");
        return it->value.second;
    }

    Value& operator[](const Key& key) {
        auto [it, res] = myFind(key);
        if (!res)
            it = inPlaceInsert(it, NodeType{ key, Value() });
        return it->value.second;
    }

    Value& operator[](Key&& k_rv) {
        Key key = std::move(k_rv);
        auto [it, res] = myFind(k);
        if (!res)
            it = inPlaceInsert(it, NodeType{ std::move(key), Value() });
        return it->value.second;
    }

    std::pair<iterator, bool> insert(const NodeType& value) {
        auto* tmp = correctConstruct(value);
        std::pair<iterator, bool> result;
        try {
            result = insert(std::move(*tmp));
        }
        catch (...) {
            AllocTraits::deallocate(allocator, tmp, 1);
            throw;
        }
        AllocTraits::deallocate(allocator, tmp, 1);
        return result;
    }

    std::pair<iterator, bool> insert(NodeType&& value) {
        auto [it, res] = myFind(value.first);
        if (res)
            return { iterator(it), false };
        LIterator result;
        result = inPlaceInsert(it, std::move(value));
        return { iterator(result), true };
    }

    template<typename InputIt>
    void insert(InputIt first, InputIt last) {
        while (first != last) {
            insert(*first);
            ++first;
        }
    }

    template<typename... Args>
    std::pair<iterator, bool> emplace(Args&&... args) {
        auto* tmp = correctConstruct(std::forward<Args>(args)...);
        std::pair<iterator, bool> result;
        try {
            result = insert(std::pair<const Key, Value>{std::move(const_cast<Key&>(tmp->first)), std::move(tmp->second)});
        }
        catch (...) {
            AllocTraits::deallocate(allocator, tmp, 1);
            throw;
        }
        AllocTraits::deallocate(allocator, tmp, 1);
        return result;
    }

    iterator erase(const_iterator pos) {
        auto lit = pos.it;
        auto hash = lit->hash;
        if (*table[hash % table.size()] == lit) {
            auto nlit = lit;
            ++nlit;
            if (nlit->hash % table.size() != hash % table.size())
                table[hash % table.size()] = std::nullopt;
            else
                table[hash % table.size()] = nlit;
        }
        lit = bucket.erase(lit);
        return iterator(lit);
    }

    iterator erase(const_iterator first, const_iterator last) {
        while (first != last) {
            first = erase(first);
        }
        return iterator(first.it);
    }

    iterator find(const Key& key) {
        auto [it, res] = myFind(key);
        if (!res)
            return end();
        return iterator(it);
    }

    const_iterator find(const Key& key) const {
        auto [it, res] = myFind(key);
        if (!res)
            return end();
        return const_iterator(it);
    }

    Alloc get_allocator() const {
        return allocator;
    }

    float load_factor() const {
        return static_cast<float>(bucket.size()) / static_cast<float>(table.size());
    }

    float max_load_factor() const {
        return max_lf;
    }

    void max_load_factor(float ml) {
        max_lf = ml;
    }

    size_t size() const {
        return bucket.size();
    }

    auto begin() {
        return iterator(bucket.begin());
    }
    auto end() {
        return iterator(bucket.end());
    }
    auto begin() const {
        return cbegin();
    }
    auto end() const {
        return cend();
    }
    auto cbegin() const {
        return const_iterator(LIterator(bucket.cbegin().ptr));
    }
    auto cend() const {
        return const_iterator(LIterator(bucket.cend().ptr));
    }
};

