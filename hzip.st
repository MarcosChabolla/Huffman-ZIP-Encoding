#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Amit Khatri: 1398993"
"Marcos Chabolla: 1437530"
nl := Character nl.


Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

Object subclass: BitStream [
    |stringIndex bitIndex byte myStream writeIndex writeByte|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
      writeIndex := 9.
      writeByte := 0.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
   
    putBit: theBit [
      writeIndex := writeIndex - 1.
      writeByte := writeByte bitAt: writeIndex put: theBit.
      writeIndex = 1 ifTrue: [
         myStream nextPutByte: writeByte.
         writeByte := 0.
         writeIndex := 9.
].
   ]
   
   putString: theString [
      theString do: [:index|
         (index asString = '0')
            ifTrue: [self putBit: 0]
            ifFalse: [self putBit: 1].
      ].
   ]
   padOut [
      [writeIndex > 1 & writeIndex < 9] whileFalse: [ 
         self putBit: 0. 
         writeIndex := writeIndex - 1.].
   ]

]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [ ^ true ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   treeBitstream: aBitstream [  
      aBitstream putBit: 0.
      (1 to: 8) do: [:index | 
          aBitstream putBit: ( (self char asInteger) bitAt: index) ].
      char asInteger = 0 ifTrue: [
         aBitstream putBit: 0.
      ].
      char asInteger = 256 ifTrue: [
         aBitstream putBit: 1.
      ]

    ]

]


Leaf subclass: Tree [
   |left right|
   left [ ^ left]
   right [ ^ right]
   isLeaf [ ^ false]

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
    treeBitstream: aBitstream [
      left treeBitstream: aBitstream.
      right treeBitstream: aBitstream.
      aBitstream putBit: 1.
   ]

]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]


Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]
]


FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
    parseFile [
      |freqtable|
      freqtable := ZeroArray new: 257.
      [self atEnd not] whileTrue: [
         |nextChar|
         nextChar := self next asInteger.
         freqtable incr: nextChar.
      ].
      freqtable incr: 256.
      ^ freqtable
    ]
]


infile := ''.
outfile := stdout.
dFlag := false.
cFlag := false.
tFlag := false.
uFlag := false.
nl := Character nl.

(1 to: Smalltalk arguments size)
do: [:index |
    argu := (Smalltalk arguments at: index).
    (argu includes: $-) ifTrue: [
        (argu includes: $d) ifTrue: [dFlag := true.].
        (argu includes: $c) ifTrue: [cFlag := true.].
        (argu includes: $t) ifTrue: [tFlag := true.].
        (argu includes: $u) ifTrue: [uFlag := true.].
    ] ifFalse: [
        (infile isEmpty) ifTrue: [
            infile := argu.
        ] ifFalse: [
            outfile :=  argu.
        ]
    ]
].



|readfile|
readfile := FileStream open: infile 
                       mode: FileStream read.

writefile := FileStream open: outfile
                        mode: FileStream write.

|frequencyTable|
frequencyTable := readfile parseFile.

|priorityQ|
priorityQ := SortedCollection new.

uFlag ifFalse:[
   (0 to: frequencyTable size - 1)
   do: [ :index |
      |aLeaf aCount aChar|
      aCount := frequencyTable at: index.
      aCount isInteger ifTrue: [
          aChar := index asCharacter.
          aLeaf := Leaf new: aChar  count: aCount.
          priorityQ add: aLeaf.
      ].
   ].
].

dFlag ifTrue: [priorityQ inspect.].

uFlag ifFalse: [
   [priorityQ size > 1] whileTrue: [
       |last nextLast aTree treeVal treeCount|
       last := priorityQ removeFirst.
       nextLast := priorityQ removeFirst.
       treeVal := last char.
       treeCount := last count + nextLast count.
       aTree := Tree new: treeVal count: treeCount left: last 
                                                    right: nextLast.
       priorityQ add: aTree.
   ].
].





|theTree encodeTable|
uFlag ifFalse: [ theTree := priorityQ removeLast.].
encodeTable := ZeroArray new: 257.
uFlag ifFalse: [
   theTree inorder: [:char :string |
       |index count|
       index := char asInteger.
       encodeTable at: index put: string. 
   ] prefix: ''.
].

tFlag ifTrue: [ "Print to stdout"
   stdout<<'-----------------'<<nl.
   stdout<<'ENCODING TABLE'<<nl.
   (0 to: encodeTable size - 1)
   do: [:index |
       |char over32 under127 count string|
       over32 := index > 32.
       under127 := index < 127.
       count := frequencyTable at: index.
       string := encodeTable at: index.
       count isInteger ifTrue: [
          over32 & under127 ifTrue: [
             char := index asCharacter.
             stdout << char << ' ' << count << ' ' << string << nl.
          ] ifFalse: [
             stdout << index << ' ' << count << ' ' << string << nl.
          ].
       ].
   ]. 
   stdout<<'-----------------'<<nl.
   readfile close.
   
   stdout<<nl<<nl.
   stdout<<'-----------------'<<nl.
   stdout<<'TREE'<<nl.
   theTree inspect.
   stdout<<'-----------------'<<nl.
].

cFlag ifTrue: [ "Compression Algorithm"
   |encString|
   aBitstream := BitStream new: writefile.
   readfile reset.
   theTree treeBitstream: aBitstream.
   aBitstream putBit: 1. 
   [readfile atEnd not] whileTrue: [
      |index|
      index := readfile next asInteger.
      encString := (encodeTable at: index).
      aBitstream putString: encString.
      
   ].
   encString := encodeTable at: 256.
   aBitstream putString: encString.
   aBitstream padOut.
   readfile close.
   writefile close.
].


uFlag ifTrue: [ "Decompression Algorithm"
   readfile := FileStream open: infile 
                       mode: FileStream read.
   outBitstream := BitStream new: readfile.
   inBitstream := BitStream new: writefile.

   theStack := OrderedCollection new.
   treeDone := false.
   decodeTree := nil.
    
   [(outBitstream atEnd not) & (treeDone = false)] whileTrue: [
      
      |nextChar|
      nextChar := outBitstream nextBit.
      
      nextChar = 0 ifTrue: [
          realChar := 0.
          basetwo := 1.
          
          (1 to: 8) do: [:skip |
             |currbit|
             currbit := outBitstream nextBit.
             (currbit = 1)
                 ifTrue: [realChar := realChar + basetwo.].
              basetwo := basetwo *2.
          ].
          
          realChar = 0 ifTrue: [
              |currbit|
              currbit := outBitstream nextBit.
              currbit = 1 ifTrue: [theStack addFirst: 
              (Leaf new: 256 asCharacter count: 1).]
                          ifFalse: [theStack addFirst: 
                          (Leaf new: 0 asCharacter count: 1).].
          ] ifFalse: [theStack addFirst: 
          (Leaf new: realChar asCharacter count: 1).].
      ] ifFalse: [
         theStack size > 0 ifTrue: [
            right := theStack removeFirst.
            theStack size = 0 ifTrue: [
               decodeTree := right.
               treeDone := true.
            ] ifFalse: [
               left := theStack removeFirst.
               theStack addFirst: 
               (Tree new: (left char) count: 1 left: left right: right).
            ].
          ].
       ].
   ].
   
   writefile := FileStream open: outfile
                        mode: FileStream write.
   tempTree := decodeTree.
   eof := false.
   
   [(outBitstream atEnd not)] whileTrue: [
      |readbit|
      readbit := outBitstream nextBit.
      readbit = 0 ifTrue: [
          tempTree := tempTree left.
          tempTree isLeaf ifTrue: [
           writefile << tempTree char.
            tempTree := decodeTree.
          ]
      ].
      readbit = 1 ifTrue: [
          tempTree := tempTree right.
          tempTree isLeaf ifTrue: [
              writefile << tempTree char.
              tempTree := decodeTree.
          ]
      ]. 
   ].
   
   readfile close.
   writefile close.
].
