module seaweeds
    implicit none
    private
    public :: seaweed

    type seaweed
        private
    contains
        procedure :: assign
        generic :: assignment(=) => assign
    end type

contains
    subroutine assign(this, other)
        implicit none
        class(seaweed), intent(out) :: this
        type(seaweed), intent(in) :: other
    end subroutine
end module
