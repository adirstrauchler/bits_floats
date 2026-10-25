#include "assignment.h"

#include <math.h>
#include <stddef.h>

#include <stdio.h>
#include <string.h>

unsigned bitset_symmetric_difference_count(uint32_t a, uint32_t b)
{
    uint32_t set = a ^ b;
    int count = 0;
    uint32_t del = 4294967294;
    for (int i = 0; i < 32; i++)
    {
        uint32_t temp = set;
        temp = temp | del;
        temp = ~temp;
        if (temp == 0)
        {
            count++;
        }
        else
        {
        }
        set = set >> 1;
    }

    return count;
}

uint32_t bit_range_mask(unsigned low, unsigned high)
{
    uint32_t result = 4294967295;
    if (high == 32)
    {
        return result;
    }
    uint32_t right = result >> (32 - high - 1);
    uint32_t left = result << (low);
    result = right & left;

    return result;
}

uint32_t splice_bits(
    uint32_t destination,
    uint32_t source,
    unsigned low,
    unsigned high)
{
    if (high == 32)
    {
        return source;
    }
    uint32_t mask = ~bit_range_mask(low, high);
    destination = destination & mask;
    uint32_t sourceMask = bit_range_mask(0, high - low);
    source = source & sourceMask;
    source = source << (low);

    return source | destination;
}
int32_t decode_twos_complement(uint32_t bits, unsigned width)
{

    uint32_t mask = bit_range_mask(0, width - 1);
    bits = bits & mask;
    if (bits == 0)
    {
        return 0;
    }
    uint32_t sign = (bits >> (width - 1));
    bool positive = !(sign);
    uint32_t result = 0;
    uint32_t u = 1;
    uint32_t del = 4294967294;
    if (!positive)
    {
        bits = ~bits;
        bits = bits & mask;
        bits = bits + 1;
        bits = bits & mask;
    }
    for (unsigned i = 0; i < width; i++)
    {
        uint32_t temp = bits;
        temp = temp | del;
        temp = ~temp;
        if (temp == 0)
        {
            result = result + u;
        }
        bits = bits >> 1;
        u = u * 2;
    }
    if (positive)
    {
        return result;
    }
    else
    {
        return -result;
    }
}

bool add_signed_width(
    uint32_t a_bits,
    uint32_t b_bits,
    unsigned width,
    uint32_t *sum_bits)
{

    if (sum_bits != NULL)
    {
        *sum_bits = 0;
    }
    uint32_t mask = bit_range_mask(0, width - 1);
    a_bits = a_bits & mask;
    b_bits = b_bits & mask;
    uint32_t a_sign = (a_bits >> (width - 1));
    uint32_t b_sign = (b_bits >> (width - 1));
    uint32_t result = a_bits + b_bits;
    uint32_t r_sign = (result >> (width - 1));
    r_sign = r_sign & bit_range_mask(0, 0);
    result = result & mask;
    *sum_bits = result;
    if ((a_sign == b_sign) && (a_sign != r_sign))
    {
        return true;
    }
    return false;
}

int compare_signed_unsigned_math(
    int32_t signed_value,
    uint32_t unsigned_value)
{
    uint32_t negative = (signed_value >> 31) & bit_range_mask(0, 0);
    if (negative)
    {
        return -1;
    }
    else
    {
        uint32_t positive = (uint32_t)signed_value;
        if (positive == unsigned_value)
        {
            return 0;
        }
        else if (positive < unsigned_value)
        {
            return -1;
        }
        else
        {
            return 1;
        }
    }
}

uint32_t multiply_by_24(uint32_t x)
{
    uint32_t thirty_two = x << 5;
    uint32_t eight = x << 3;
    return thirty_two - eight;
}

fp32_info_t fp32_decode(uint32_t bits)
{

    fp32_info_t info = {
        .negative = false,
        .kind = FP32_ZERO,
        .exponent_bits = 0,
        .fraction_bits = 0,
        .unbiased_exponent = -126,
    };
    uint32_t sign_bit = bits & bit_range_mask(31, 31);
    bool sign = sign_bit;
    info.negative = sign;

    uint8_t exponent = bits >> 23;
    info.exponent_bits = exponent;

    uint32_t fraction = bits & bit_range_mask(0, 22);
    info.fraction_bits = fraction;

    if (info.exponent_bits == 0 && info.fraction_bits == 0)
    {
        info.kind = FP32_ZERO;
    }
    else if (info.exponent_bits == 0 && info.fraction_bits != 0)
    {
        info.kind = FP32_SUBNORMAL;
    }
    else if (info.exponent_bits < 255)
    {
        info.kind = FP32_NORMAL;
    }
    else if (info.exponent_bits == 255 && info.fraction_bits == 0)
    {
        info.kind = FP32_INFINITY;
    }
    else
    {
        info.kind = FP32_NAN;
    }

    if (info.kind == FP32_NORMAL)
    {
        info.unbiased_exponent = info.exponent_bits - 127;
    }
    else if (info.kind == FP32_SUBNORMAL || info.kind == 0)
    {
        info.unbiased_exponent = -126;
    }
    else
    {
        info.unbiased_exponent = 0;
    }
    return info;
}

double fp32_exact_value(uint32_t bits)
{
    fp32_info_t info = fp32_decode(bits);
    double value;
    double sign = 0;
    if (info.negative)
    {
        sign = -1;
    }
    else
    {
        sign = 1;
    }
    if (info.kind == FP32_INFINITY)
    {
        if (info.negative)
        {
            return -1.0 / 0.0;
        }
        else
        {
            return 1.0 / 0.0;
        }
    }
    else if (info.kind == FP32_NAN)
    {
        return 0.0 / 0.0;
    }
    uint32_t fraction_bits = info.fraction_bits;
    double fraction = (double)info.fraction_bits;
    uint32_t del = 4294967294;
    fraction = 0;
    for (double i = pow(2, 23); i > 0; i = i / 2)
    {
        uint32_t temp = fraction_bits;
        temp = temp | del;
        temp = ~temp;
        if (temp == 0)
        {
            fraction = fraction + 1 / i;
        }
        fraction_bits = fraction_bits >> 1;
    }
    if (info.kind == FP32_NORMAL)
    {
        fraction = fraction + 1;
        value = (sign * fraction * pow(2, (info.exponent_bits - 127)));
    }
    else
    {
        value = (sign * fraction * pow(2, -126));
    }
    return value;
}

uint32_t fp32_next_up_bits(uint32_t bits)
{
    fp32_info_t info = fp32_decode(bits);
    if (info.kind == FP32_ZERO)
    {
        return 1;
    }
    else if (info.kind == FP32_NAN)
    {
        return bits;
    }
    else if (info.kind == FP32_INFINITY && !info.negative)
    {
        return bits;
    }
    if (info.negative)
    {
        return bits - 1;
    }
    else
    {
        return bits + 1;
    }
    (void)bits;
}

void print(uint32_t result)
{
    uint32_t del = 2147483647;
    for (int i = 0; i < 32; i++)
    {
        uint32_t temp = result;
        temp = temp | del;
        temp = ~temp;
        if (temp == 0)
        {
            printf("%d", 1);
        }
        else
        {
            printf("%d", 0);
        }
        result = result << 1;
    }
    printf("\n");
}

bool fp32_truncate_to_int(uint32_t bits, int32_t *out)
{
    if (out != NULL)
    {
        *out = 0;
    }
    fp32_info_t info = fp32_decode(bits);
    if (info.kind == FP32_NAN || info.kind == FP32_INFINITY)
    {
        return false;
    }
    if (info.kind == FP32_NORMAL)
    {
        uint32_t fraction = info.fraction_bits;
        uint8_t exponent = info.exponent_bits;
        uint8_t sign = 0;
        if (!info.negative)
        {
            sign = 1;
        }
        if (exponent < 127)
        {
            fraction = 0;
            *out = 0;
            return true;
        }
        else
        {

            if (exponent > (127 + 30) && sign)
            {
                return false;
            }
            else if (exponent > (127 + 30) && fraction)
            {
                return false;
            }
            exponent = exponent - 127;
            fraction = fraction | bit_range_mask(23, 23);
            if (exponent > 23)
            {
                fraction = fraction << -(23 - exponent);
            }
            else
            {
                fraction = fraction >> (23 - exponent);
            }
            if (sign)
            {
                *out = fraction;
            }
            else
            {
                *out = -fraction;
            }
            return true;
        }
    }
    else
    {
        *out = 0;
        return true;
    }
    return false;
}