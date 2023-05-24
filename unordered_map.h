#pragma once

#include <algorithm>
#include <unordered_map>
#include <functional>


template<typename Key,
         typename Value,
         typename Hash = std::hash<Key>,
         typename Equal = std::equal_to<Key>,
         typename Alloc = std::allocator< std::pair<const Key, Value> >
>
using UnorderedMap = std::unordered_map<Key, Value, Hash, Equal, Alloc>;

