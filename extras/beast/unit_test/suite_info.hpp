//
// Copyright (c) 2013-2016 Vinnie Falco (vinnie dot falco at gmail dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef BEAST_UNIT_TEST_SUITE_INFO_HPP
#define BEAST_UNIT_TEST_SUITE_INFO_HPP

#include <cstring>
#include <functional>
#include <string>
#include <utility>

namespace beast {
namespace unit_test {

class runner;

/** Associates a unit test type with metadata. */
class suite_info
{
private:
    using run_type = std::function <void (runner&)>;

    std::string name_;
    std::string module_;
    std::string library_;
    bool m_manual;
    run_type m_run;

public:
    suite_info(
            std::string name,
            std::string module,
            std::string library,
            bool manual,
            run_type run)
        : name_(std::move(name))
        , module_(std::move(module))
        , library_(std::move(library))
        , m_manual(manual)
        , m_run(std::move(run))
    {
    }

    std::string const&
    name() const
    {
        return name_;
    }

    std::string const&
    module() const
    {
        return module_;
    }

    std::string const&
    library() const
    {
        return library_;
    }

    /// Returns `true` if this suite only runs manually.
    bool
    manual() const
    {
        return m_manual;
    }

    /// Return the canonical suite name as a string.
    std::string
    full_name() const
    {
        return library_ + "." + module_ + "." + name_;
    }

    /// Run a new instance of the associated test suite.
    void
    run(runner& r) const
    {
        m_run (r);
    }
};

//------------------------------------------------------------------------------

inline
bool
operator<(suite_info const& lhs, suite_info const& rhs)
{
    auto const& ls = lhs.full_name();
    auto const& rs = rhs.full_name();
    return std::strcmp(ls.c_str(), rs.c_str()) < 0;
}

/// Convenience for producing suite_info for a given test type.
template<class Suite>
suite_info
make_suite_info(
    std::string name,
    std::string module,
    std::string library,
    bool manual)
{
    return suite_info(
        std::move(name),
        std::move(module),
        std::move(library),
        manual,
        [](runner& r)
        {
            Suite{}(r);
        }
    );
}

} // unit_test
} // beast

#endif
