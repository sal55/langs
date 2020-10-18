static double test_evalx(byte * expr) {
    double x;
    test_lxptr = expr;
    test_nexttk();
    x = test_evalexpr();
    if ((test_token != (i64)9)) {
        test_error((byte*)"Bad ending");
    };
    return x;
}

static double test_evalexpr(void) {
    return test_evaladd();
}

static double test_evaladd(void) {
    double x;
    double y;
    i64 opc;
    x = test_evalmul();
    L1 :;
    while (((test_token == (i64)1) || (test_token == (i64)2))) {
        opc = test_token;
        test_nexttk();
        y = test_evalmul();
        if ((opc == (i64)1)) {
            x += y;
        } else {
            x -= y;
        };
L2 :;
    }L3 :;
    ;
    return x;
}

static double test_evalmul(void) {
    double x;
    double y;
    i64 opc;
    x = test_evalterm();
    L4 :;
    while (((test_token == (i64)3) || (test_token == (i64)4))) {
        opc = test_token;
        test_nexttk();
        y = test_evalterm();
        if ((opc == (i64)3)) {
            x *= y;
        } else {
            if ((y == (double)0.)) {
                test_error((byte*)"Divide by zero");
            };
            x /= y;
        };
L5 :;
    }L6 :;
    ;
    return x;
}

static double test_evalterm(void) {
    double x;
    if ((test_token==(i64)2)) {
        test_nexttk();
        return -(test_evalterm());
    }else if ((test_token==(i64)5)) {
        test_nexttk();
        x = test_evalexpr();
        if ((test_token != (i64)6)) {
            test_error((byte*)"')' expected");
        };
        test_nexttk();
        return x;
    }else if ((test_token==(i64)8)) {
        test_nexttk();
        return sin(test_evalterm());
    }else if ((test_token==(i64)7)) {
        x = test_tkvalue;
        test_nexttk();
        return x;
    } else {
        test_error((byte*)"Term");
    };
    return (double)0.;
}

static void test_nexttk(void) {
    i64 c;
    byte *  pstart;
    switch ((c = (i64)((*test_lxptr++)))) {
    case 32:;
    case 9:;
    {
        test_nexttk();
    }break;
    case 43:;
    {
        test_token = (i64)1;
    }break;
    case 45:;
    {
        test_token = (i64)2;
    }break;
    case 42:;
    {
        test_token = (i64)3;
    }break;
    case 47:;
    {
        test_token = (i64)4;
    }break;
    case 40:;
    {
        test_token = (i64)5;
    }break;
    case 41:;
    {
        test_token = (i64)6;
    }break;
    case 65:;
    case 66:;
    case 67:;
    case 68:;
    case 69:;
    case 70:;
    case 71:;
    case 72:;
    case 73:;
    case 74:;
    case 75:;
    case 76:;
    case 77:;
    case 78:;
    case 79:;
    case 80:;
    case 81:;
    case 82:;
    case 83:;
    case 84:;
    case 85:;
    case 86:;
    case 87:;
    case 88:;
    case 89:;
    case 90:;
    {
        test_tkvalue = test_variables[((c + (i64)32))-97];
        test_token = (i64)6;
    }break;
    case 97:;
    case 98:;
    case 99:;
    case 100:;
    case 101:;
    case 102:;
    case 103:;
    case 104:;
    case 105:;
    case 106:;
    case 107:;
    case 108:;
    case 109:;
    case 110:;
    case 111:;
    case 112:;
    case 113:;
    case 114:;
    case 115:;
    case 116:;
    case 117:;
    case 118:;
    case 119:;
    case 120:;
    case 121:;
    case 122:;
    {
        if ((((c == (i64)115) && ((u64)((*test_lxptr)) == 'i')) && ((u64)((*(test_lxptr + (i64)1))) == 'n'))) {
            test_lxptr += (i64)2;
            test_token = (i64)8;
        } else {
            test_tkvalue = test_variables[(c)-97];
            test_token = (i64)7;
        };
    }break;
    case 46:;
    case 48:;
    case 49:;
    case 50:;
    case 51:;
    case 52:;
    case 53:;
    case 54:;
    case 55:;
    case 56:;
    case 57:;
    {
        pstart = (test_lxptr - (i64)1);
        test_tkvalue = strtod((i8 *)(pstart),(void *)(&test_lxptr));
        test_token = (i64)7;
    }break;
    case 0:;
    {
        test_token = (i64)9;
    }break;
    default: {
        test_error((byte*)"Syntax");
    }
    } //SW
;
}

static void test_error(byte * mess) {
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"Error:",NULL);
    msysnewc_m_print_str(mess,NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
    exit((i64)1);
}

// START
void start(void) {
    test_variables[((i64)120)-97] = (double)0.52300000000000002;
    test_variables[((i64)121)-97] = (double)34.600000000000001;
    test_variables[((i64)122)-97] = (double)99.;
    msysnewc_m_print_startcon();
    msysnewc_m_print_str((byte*)"EVALX(\"SIN(X)*2+Y\")=",NULL);
    msysnewc_m_print_r64(test_evalx((byte*)"sin(x)*2+y"),NULL);
    msysnewc_m_print_newline();
    msysnewc_m_print_end();
    ;
}
