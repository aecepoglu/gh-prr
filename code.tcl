package require json
package require term::ansi::ctrl::unix
package require term::ansi::code::attr

set myName aecepogluARUP
set repo aecepoglu/gh-prr
set pr 13

proc group-by {f xs} {
		set y [dict create]
		foreach x $xs {
			set v [$f $x]
			dict lappend y $v $x
		}
		return $y
}
proc dict-get-either {d a b} { if [dict exists $d $a] then {return [dict get $d $a]} else {return [dict get $d $b] } }
proc uniq {xs} { set y {}; foreach x $xs { dict set y $x "" }; dict keys $y}
proc iota {a b {d 1}} { set y {}; for {set i $a} {$i < $b} {incr i $d} {lappend y $i}; return $y }
proc max {a b} {expr $a > $b ? $a : $b}
proc min {a b} {expr $a < $b ? $a : $b}

proc readraw {} {
	term::ansi::ctrl::unix::raw
	set x [read stdin 1]
	term::ansi::ctrl::unix::cooked
	return $x
}
proc prompt {txt} {
	puts -nonewline $txt
	flush stdout
	if [string equal [set x [readraw]] \n] then { puts "<ret>" } else { puts $x }
	return $x
}
proc wrap {line width} {
	set lines {}
	set n [string length $line]
	set idxs [concat [iota 0 $n $width] $n]
	for {set i 0; set j 1} {$j < [llength $idxs]} {incr i 1; incr j 1} {
		lappend lines [string range $line [lindex $idxs $i] [expr [lindex $idxs $j] - 1]]
	}
	return $lines
}
proc printDiff {comment {fp stdout}} {
	global attr
	set i0 [dict get $comment original_start_line];
	set iN [dict get $comment original_line]
	set k [expr 2 + round(log($iN)/log(10))]
	set width [expr [term::ansi::ctrl::unix::columns] - [expr $k + 2]]
	set lines [split [dict get $comment diff_hunk] \n] 
	set fmt [string cat %- $k d]
	regexp {^@@ -([\d]+),[\d]+ \+[\d]+,[\d]+ @@} [lindex $lines 0] _ i
	puts "[string repeat { } $k]┌╴ $attr(bold)diff$attr(reset)"
	foreach line $lines {
		set gutter [format $fmt $i]
		if {$i >= $i0 && $i <= $iN} { set border "┃" } else { set border "╎" }
		foreach x [wrap $line $width] b {1} {
			puts "$gutter$border $x"
			set gutter [string repeat " " $k]
		}
		incr i
	}
}
proc printDebug {comment} {
	global attr
	dict for {k v} $comment { puts "$attr(bold)$k:$attr(reset) $v" }
}
proc kakEval {session client txt} {
	exec kak -p $session << "eval -client $client %{$txt}"
}

# set data [exec gh api -H "Accept: application/vnd.github+json" "/repos/$repo/pulls/$pr/comments"]
foreach x {reset bold italic fgred fggreen fgblue} {
	set attr($x) [term::ansi::code::escb "[term::ansi::code::attr::$x]m"] }
set data [exec cat test.json]
set threads [dict values [group-by [proc "" {d} {dict-get-either $d in_reply_to_id id}] [json::json2dict $data]]]
set users [uniq [concat $myName {*}[lmap t $threads { lmap x $t { dict get $x user login }}]]]
for {set i 0; set c [array get attr fg*]} {$i < [llength $users]} {incr i} {
	 set colors([lindex $users $i]) [lindex $c [expr $i*2+1]] }

set actions [dict create {*}{
	n {next {set threadI [min [expr $threadN - 1] [expr $threadI + 1]]}}
	p {prev {set threadI [max 0                   [expr $threadI - 1]]}}
	q {quit {set threadI -1 }}
	e {edit {set comment [lindex $thread 0]
			kakEval wss client0 "edit -existing [dict get $comment path] [dict get $comment original_line] [dict get $comment original_position]"
			puts "opened in kakoune session 'wss' client 'client0'"
		}}
	v {view {printDiff [lindex $thread 0]}}
	d {debug {printDebug [lindex $thread 0]}}
	r {reply {
			global pr
			set file [exec mktemp -u tmp.XXXX.fifo]
			exec mkfifo $file
			kakEval wss client0 "edit -fifo $file *pr-$pr/reply*"
			set fp [open $file w]
			puts $fp "### add your reply above this line"
			printThread $threadN $threadI $thread $fp
			close $fp
		}}
	a {apply {puts "TODO apply"}}
}]

proc printThread {n i t {fp stdout}} {
	global colors; global attr; global actions
	set x0 [lindex $t 0]
	puts $fp "$i. [dict get $x0 path]:[dict get $x0 original_start_line]-[dict get $x0 original_line]"
	set myActions {n p q e r v d}
	for {set j 0} {$j < [llength $t]} {incr j} {
		set x [lindex $t $j]; set u [dict get $x user login]
		set s [dict get $x body]
		puts $fp "$colors($u)$u$attr(reset) on [dict get $x updated_at]: $s"
		if {[string first "```suggestion\r\n" $s] >= 0} {
			printDiff $x $fp
			lappend myActions a
		}
	}
	return $myActions
}
proc itercomments {} {
	global threads; global actions
	set threadN [expr [llength $threads] - 1]
   set sep [string repeat "═" [term::ansi::ctrl::unix::columns]]
	for {set threadI 0; set threadIprev -1; set prompts ""} {$threadI >= 0 && $threadI < $threadN} {} {
		set thread [lindex $threads $threadI]
		if {$threadI != $threadIprev} {
			puts $sep
			set prompts [lmap x [printThread $threadN $threadI $thread] {
				lindex [dict get $actions $x] 0}]
		}
		set threadIprev $threadI

		set key [prompt "{$threadI/$threadN} $prompts: "]
		if [string equal $key \n] { set key n }

		eval [lindex [dict get $actions $key] 1]
	}
}

itercomments
