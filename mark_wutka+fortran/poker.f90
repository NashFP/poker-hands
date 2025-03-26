module poker
contains

! Converts a char to a rank between 0 and 12 
  function char_to_rank(ch)
    character :: ch
    integer :: char_to_rank

    select case (ch)
    case ('2':'9')
      char_to_rank = iachar(ch) - iachar('2')
    case ('T')
      char_to_rank = 8
    case ('J')
      char_to_rank = 9
    case ('Q')
      char_to_rank = 10
    case ('K')
      char_to_rank = 11
    case ('A')
      char_to_rank = 12
    case default
      stop "Invalid rank"
    end select
  end function char_to_rank

! Converts a char to a suit between 0 and 3
  function char_to_suit(ch)
    character :: ch
    integer :: char_to_suit

    select case (ch)
    case ('H')
      char_to_suit = 0
    case ('D')
      char_to_suit = 1
    case ('S')
      char_to_suit = 2
    case ('C')
      char_to_suit = 3
    case default
      stop "Invalid suit"
    end select
  end function char_to_suit

! Given and rank and suit encoded in a card val, return the rank
  function rank(card)
    integer rank, card

    rank = mod(card, 13)
  end function rank

! Given and rank and suit encoded in a card val, return the suit
  function suit(card)
    integer suit, card
    
    suit = card / 13
  end function suit

! Convert an array of card ranks into a single integer
  function ranks_to_int(ranks)
    integer :: ranks_to_int
    integer, dimension(5) :: ranks
    integer :: i

    ranks_to_int = 0
    do i=1,5
      ranks_to_int = ranks_to_int * 13 + ranks(i)
    end do
  end function ranks_to_int

! Parse a hand into an array of 5 card values
  subroutine parse_hand(line, start, hand)
    character(len=*), intent(in) :: line
    integer, intent(in) :: start
    integer, dimension(5), intent(out) :: hand
    integer :: card, rank, suit, pos

    do card=1, 5
      pos = (card-1) * 3 + start
      rank = char_to_rank(line(pos:pos+1))
      suit = char_to_suit(line(pos+1:pos+2))

      hand(card) = suit * 13 + rank
    end do
  end subroutine parse_hand

! Do a simple bubble sort on an array of counts in descending order
  subroutine sort(counts)
    integer, dimension(5) :: counts
    integer :: i, j, temp

    do i=1,4
      do j=i+1,5
        if (counts(i) < counts(j)) then
          temp = counts(i)
          counts(i) = counts(j)
          counts(j) = temp
        end if
      end do
    end do
  end subroutine sort

! Return a single integer indicating the score of a hand
  function score_hand(hand)
    implicit none
    integer :: score_hand
    integer, dimension(5) :: hand, hand_counts, ranks
    integer, dimension(13) :: counts
    logical :: is_flush, is_straight
    integer i, pos, first_count, num_unique, rank_val, curr_suit
    integer hand_type

! Define the constants for the base score for each hand type
    integer, parameter :: hand_size = 13 * 13 * 13 * 13 * 13
    integer, parameter :: high_card = 0
    integer, parameter :: one_pair = hand_size
    integer, parameter :: two_pairs = 2 * hand_size
    integer, parameter :: three_of_a_kind = 3 * hand_size
    integer, parameter :: straight = 4 * hand_size
    integer, parameter :: flush = 5 * hand_size
    integer, parameter :: full_house = 6 * hand_size
    integer, parameter :: four_of_a_kind = 7 * hand_size
    integer, parameter :: straight_flush = 8 * hand_size

    is_flush = .true.
    curr_suit = suit(hand(1))
    counts = 0

! Count how many times each card occurs, and alse see if
! this hand contains a flush
    do i=1,5
      rank_val = rank(hand(i))
      counts(rank_val+1) = counts(rank_val+1) + 1
      is_flush = is_flush .and. suit(hand(i)) .eq. curr_suit
    end do

! Look through the counts and find the cards with a non-zero
! count. Encode both the count and the rank as count * 13 + rank.
! Also count how many unique cards there were
    pos = 1
    num_unique = 0
    hand_counts = 0
    do i=1,13
      if (counts(i) > 0) then
        hand_counts(pos) = counts(i) * 13 + i - 1
        pos = pos + 1
        num_unique = num_unique + 1
      end if
    end do

! Sort the counts in descending order
    call sort(hand_counts)

! first_count the maximum number of repeated cards
    first_count = hand_counts(1) / 13

! Get an array of card ranks in order of most-frequent card first
    do i=1,5
      ranks(i) = mod(hand_counts(i), 13)
    end do
      
! This is a straight if all the cards are unique and the last card
! is exactly 4 less than the first card.
    is_straight = num_unique .eq. 5 .and. ranks(1)-ranks(5) .eq. 4

! Figure out the hand type based on the number of unique
! cards, the flush/straight flags and first_count
    select case(num_unique)
    case (5)
      if (is_flush .and. is_straight) then
        hand_type = straight_flush
      else if (is_flush) then
        hand_type = flush
      else if (is_straight) then
        hand_type = straight
      else
        hand_type = high_card
      end if
    case (4)
      if (is_flush) then
        hand_type = flush
      else
        hand_type = one_pair
      end if
    case (3)
      if (first_count .eq. 3) then
        hand_type = three_of_a_kind
      else
        hand_type = two_pairs
      end if
    case (2)
      if (first_count .eq. 4) then
        hand_type = four_of_a_kind
      else
        hand_type = full_house
      end if
    case default
      stop "invalid hand"
    end select

    score_hand = hand_type + ranks_to_int(ranks)
  end function score_hand

end module poker

program main
  use poker
  implicit none

  integer :: ios, num_player1_wins
  integer, parameter :: file_in = 99
  integer, dimension(5) :: hand1, hand2
  character :: line*80

  open(unit=file_in, file='../data/p054_poker.txt', iostat=ios)
  if (ios /= 0) stop "Error opening data file"

  do
    read(file_in, '(A)', iostat=ios) line
    if (ios /= 0) exit

    call parse_hand(line, 1, hand1)
    call parse_hand(line, 16, hand2)

    if (score_hand(hand1) > score_hand(hand2)) then
      num_player1_wins = num_player1_wins + 1
    end if

  end do
  print *, "Num player 1 wins = ", num_player1_wins

  close(unit=file_in)
end program main

