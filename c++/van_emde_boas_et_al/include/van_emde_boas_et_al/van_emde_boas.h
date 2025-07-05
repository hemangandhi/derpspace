#include <concepts>
#include <optional>
#include <vector>

namespace van_emde_boas_et_al {
namespace van_emde_boas {

template <std::integral K, typename V>
class VanEmdeBoasTree {
   public:
    // TODO: be smarter about allocation?
    // Really, an Entry should be K, const V& or something.
    struct Entry {
        K key;
        V value;
    };

    // TODO: const& V?
    const Entry& insert(K key, V value) {
        if (!min_max_info_) {
            Entry min{.key = key, .value = value};
            min_max_info_ = std::make_pair(min, &min);
            min_max_info_.second = &min_max_info_.first;
            return min_max_info_.first;
        }

        if (key == min_max_info_.first.key) {
            return min_max_info_.first;
        }
        if (key == min_max_info_.second->key) {
            return *min_max_info_.second;
        }

        if (key < min_max_info_.first.key) {
            Entry new_min{.key = key, .value = value};
            Entry old_min = min_max_info_.first;
            min_max_info_.first = new_min;
        }
    }

   private:
    std::optional<std::pair<Entry, Entry*>> min_max_info_;

    std::vector<Entry> children_;
};

}  // namespace van_emde_boas
}  // namespace van_emde_boas_et_al
