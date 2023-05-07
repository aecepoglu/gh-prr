#!/bin/env tclsh
package require json
package require term::ansi::ctrl::unix
package require term::ansi::code::attr

proc with-file {chanVar file mode body} {
	set c [open $file $mode]
	set x [apply [list $chanVar $body] $c]
	close $c
	return $x
}

proc kakEval {args} {
	exec kak -p wss << "eval -client client0 %{[join $args {; }]}"
}
proc kakoune {type fileVar chVar moreVars moreVals body1 body2} {
	set mode [switch $type {
		tell {set "" w}
		ask  {set "" r}
		default {return -error "Bad type: $type. It can be tell|ask"}}]
	exec mkfifo [set file [exec mktemp -u /tmp/tcl-kak.XXXX.fifo]]
	kakEval {*}[apply \
		[list [concat $moreVars $fileVar] $body1] \
		{*}[concat $moreVals $file]]
	set ch [open $file $mode]
	set x [apply \
		[list [concat $moreVars $fileVar $chVar] $body2] \
		{*}[concat $moreVals $file $ch]]
	close $ch
	file delete $file
	return $x
}

proc op {key name help body} {
	global binds helps $name
	set binds($key) $name
	set helps($key) $help
	proc $name {} [concat {global thread threadI threadN prNo attr;} $body]
}
op n op:next "goto next thread" {
	set threadI [min $threadN [expr $threadI + 1]]
}
op p op:prev "goto prev thread" {set threadI [max 0 [expr $threadI - 1]]}
op q op:quit "terminates program" {set threadI -1 }
op e op:edit "goto line in kakoune" {
	kakEval "edit -existing [dict get $thread path] [dict get $thread line]"
	puts {(opened in kakoune session "wss" client "client0")}
}
op v op:view {show relevant diff} {printDiff $thread}
op d op:debug {show some (useful?) debug info} {printDebug $thread}
op r op:replyEdit {open kakoune to create/edit a reply} {
	set buflist [kakoune ask file ch {} {} {list "echo -quoting kakoune -to-file $file %val(buflist)"} {string map {' {"} {"} \"} [read $ch]}]
	set bufname "*prr:$prNo:$threadI*"
	if [expr [lsearch $buflist $bufname] >= 0] {
		puts {found the buffer}
		kakEval "buffer $bufname"
		return
	}
	kakoune tell file ch {buf trd} [list $bufname $thread] {list "edit -fifo $file $buf"} {
		puts $ch "
%%% add your reply above this line
%   then return to `prr` and press any key to save your reply
%   It will be sent as a reply to this thread"
		printThread $trd $ch 0
	}
	puts "opened the reply in kakoune. Compose your reply and return here to send it. Press <?> for help for a reminder."
}
op R op:replySend {send reply for current thread} {
	global replies
	set bufname "*prr:$prNo:$threadI*"
	set buf [kakoune ask file ch bufname $bufname \
		{list \
			"buffer $bufname" \
			{exec <%>} \
			"echo -to-file $file %val(selection)" \
			"delete-buffer $bufname" \
			{info "your reply has been saved and its buffer closed"}} \
		{read $ch}]
	if [regexp -indices -lineanchor {^%%% add your reply above this line$} $buf x] {
		set reply [string range $buf 0 [expr [lindex $x 0] - 2]]
		set commentId [dict get [lindex [dict get $thread comments nodes] 0] databaseId]
		exec gh api -H {Accept: application/vnd.github+json} --method POST \
			"/repos/{owner}/{repo}/pulls/$prNo/comments/$commentId/replies" -f "body=$reply"
		set replies($threadI) $reply
	} else {
		puts "Reply could not be parsed. Make sure to leave the marked lines untouched"
	}
}
op w op:web "open comment in firefox" {exec firefox [dict get [lindex [dict get $thread comments nodes] end] url]}
op ? op:help "shows some usage info" {global helps; foreach {k v} [lsort -stride 2 -index 0 [array get helps]] {puts " $k  $v"} }

foreach x {reset bold italic fgred fggreen fgblue} {
	set attr($x) [term::ansi::code::escb "[term::ansi::code::attr::$x]m"] }

proc atif {cond array key} {
	if {!$cond} { return "" }
	upvar 1 $array a
	if [catch {set x $a($key)}] {
		puts "ERROR\n\n"; return ""
	} else {
		return $x
	}
}
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
proc printDiff {thread {fp stdout}} {
	global attr
	set i0 [dict get $thread startLine];
	set iN [dict get $thread line]
	set k [expr 2 + round(log($iN)/log(10))]
	set width [expr [term::ansi::ctrl::unix::columns] - [expr $k + 2]]
	set comment0 [lindex [dict get $thread comments nodes] end]
	set lines [split [dict get $comment0 diffHunk] \n] 
	set fmt [string cat %- $k d]
	regexp {^@@ -[\d]+,[\d]+ \+([\d]+),[\d]+ @@} [lindex $lines 0] _ i
	puts $fp "[string repeat { } $k]┌╴ $attr(bold)diff$attr(reset) ... $i0 ... $iN"
	foreach line $lines {
		set gutter [format $fmt $i]
		if {$i >= $i0 && $i <= $iN} { set border "┃" } else { set border "╎" }
		foreach x [wrap $line $width] b {1} {
			puts $fp "$gutter$border $x"
			set gutter [string repeat " " $k]
		}
		incr i
	}
}
proc printDebug {comment} {
	global attr
	dict for {k v} $comment { puts "$attr(bold)$k:$attr(reset) $v" }
}

proc get-pull-request {no} {
	set query { query Foo($owner: String!, $repo: String!, $pr: Int!, $endCursor:String) {
		repository(owner: $owner, name: $repo) {
			pullRequest(number: $pr) {
				participants(first:100) { nodes { login } }
				reviewDecision
				reviewThreads(first: 100, after:$endCursor) {
					nodes {
						isResolved, resolvedBy { login }, isOutdated,
						path, line, startLine
						comments(first: 100) { nodes {
							databaseId
							author { login }
							createdAt, lastEditedAt
							body
							url
							minimizedReason
							diffHunk
						}}
					}
					pageInfo { hasNextPage, endCursor }
				}
			}
		}
	}}
	set x [exec gh api graphql --paginate --cache 5m -F "owner={owner}" -F "repo={repo}" -F pr=$no -f query=$query]
	dict get [json::json2dict $x] data repository pullRequest
}
proc duration {t1 t0} {
	set s [expr {abs($t1 - $t0)}]
	if {$s < 60}                              { return {just now} }
	if {$s < 6000}                            { return "[expr $s / 60] mins ago" }
	if {[set h [expr {round($s / 3600)}]] < 48} { return "$h hours ago" }
	set d [expr {round($s / 86400)}];             return "$d days ago"
}
proc printThread {t {fp stdout} {colored 1}} {
	global colors; global attr; global actions
	set x0 [lindex $t 0]
	set t0 [clock seconds]
	puts -nonewline $fp "[dict get $t path]:[dict get $t startLine]-[dict get $t line]"
	if [dict get $t isResolved] { puts -nonewline $fp " (resolved by [dict get $t resolvedBy login])" }
	if [dict get $t isOutdated] {  puts -nonewline $fp " (outdated)" }
	puts $fp ""
	foreach c [dict get $t comments nodes] {
		set u [dict get $c author login]
		set s [dict get $c body]
		puts $fp "[atif $colored colors $u]$u[atif $colored attr reset] [duration [clock scan [dict get $c createdAt]] $t0]: $s"
	}
}

set prNo [exec gh pr view --json number --template "{{.number}}"]
set myName aecepogluARUP

set pr [get-pull-request $prNo]
set users [concat {$myName} [lmap x [dict get $pr participants nodes] { dict get $x login }]]
for {set i 0; set c [array get attr fg*]} {$i < [llength $users]} {incr i} {
	 set colors([lindex $users $i]) [lindex $c [expr $i*2+1]] }

set threads {}; foreach x [dict get $pr reviewThreads nodes] { if {![dict get $x isResolved]} {lappend threads $x} }

set threadN [expr [llength $threads] - 1]
set threadI 0; set threadIprev -1
set sep [string repeat "═" [term::ansi::ctrl::unix::columns]]
while {$threadI >= 0 && $threadI <= $threadN} {
	set thread [lindex $threads $threadI]
	if {$threadI != $threadIprev} {
		puts $sep
		printThread $thread
	}
	set threadIprev $threadI

	set key [prompt "{$threadI/[expr $threadN]} (?:help):"]
	if [string equal $key \n] { set key n }

	if {![llength [array get binds $key]]} { puts "No such key: $key"; op:help; continue }
	$binds($key)
}
#foreach r $reviews {
#	set id [dict get $r id]; puts "#$id [dict get $r user login] on [dict get $r submitted_at]: [dict get $r body]\n"
#	foreach c $comments($id) {
#		puts "x-> [dict get $c user login]"
#		puts "    [dict get $c path]:[dict get $c position]:"
#		foreach l [concat {*}[lmap x [split [dict get $c body] \n] { wrap $x 80 }]] { puts "       $l" }
#	}
#	puts "\n[dict get $r state]"
#	puts "========================"
#}
puts "Bye..."
