//
// Copyright (c) 2013-2016 Vinnie Falco (vinnie dot falco at gmail dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <beast/http/detail/chunk_encode.hpp>
#include <beast/core/to_string.hpp>
#include <beast/unit_test/suite.hpp>

namespace beast {
namespace http {
namespace detail {

class chunk_encode_test : public beast::unit_test::suite
{
public:
    static
    inline
    void
    encode(std::string& s, std::string const& fc)
    {
        using boost::asio::buffer;
        if(! fc.empty())
            s.append(to_string(
                chunk_encode(buffer(fc.data(), fc.size()))));
        s.append(to_string(chunk_encode_final()));
    }

    template<class... Args>
    static
    void
    encode(std::string& s, std::string const& arg, Args const&... args)
    {
        using boost::asio::buffer;
        s.append(to_string(
            chunk_encode(buffer(arg.data(), arg.size()))));

        encode(s, args...);
    }

    template<class... Args>
    void
    check(std::string const& answer, Args const&... args)
    {
        std::string s;
        encode(s, args...);
        expect(s == answer);
    }

    void run() override
    {
        check(
            "0\r\n\r\n"
            "0\r\n\r\n",
            "", std::string{});

        check(
            "1\r\n"
            "*\r\n"
            "0\r\n\r\n",
            "*");

        check(
            "2\r\n"
            "**\r\n"
            "0\r\n\r\n",
            "**");

        check(
            "1\r\n"
            "*\r\n"
            "1\r\n"
            "*\r\n"
            "0\r\n\r\n",
            "*", "*");

        check(
            "5\r\n"
            "*****\r\n"
            "7\r\n"
            "*******\r\n"
            "0\r\n\r\n",
            "*****", "*******");

        check(
            "1\r\n"
            "*\r\n"
            "1\r\n"
            "*\r\n"
            "0\r\n\r\n",
            "*", "*", std::string{});

        check(
            "4\r\n"
            "****\r\n"
            "0\r\n\r\n",
            "****", std::string{});
    }
};

BEAST_DEFINE_TESTSUITE(chunk_encode,http,beast);

} // detail
} // http
} // beast
