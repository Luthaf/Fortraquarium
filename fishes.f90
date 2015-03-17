module fishes
    implicit none
    private
    public :: fish

    type fish
        character(len=80) name
        logical male
    contains
        procedure :: sex => sex_str
    end type

contains
    function sex_str(this)
        implicit none
        class(fish), intent(in) :: this
        character(len=6) :: sex_str
        if (this%male) then
            sex_str = "male"
        else
            sex_str = "female"
        end if
    end function
end module
