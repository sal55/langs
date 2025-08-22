func do_bin(unit p, a, b)tclopnd =
    tclopnd ax, bx, tx

    ax := evalu(a)
    bx := evalu(b)

    case p.tclop
    when kaddpx then
        tx := tc_gen_ix(kaddpx, ax, bx, ttsize[tttarget[a.mode]])

    when ksubpx, ksubp then
        tx := tc_gent(p.tclop, ax, bx)
        tccurr.scale := ttsize[tttarget[a.mode]]

    else                            !most binary ops
        tx := tc_gent(p.tclop, ax, bx)
    esac

    setmode_u(p)
    return tx
end
