#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"------------------------------------------------"
" Zain Shafique - zshafiqu@ucsc.edu
  Jonathan Xu - jxu125@ucsc.edu
  asgn3 - CSE 112
  A compression/decompression utility based on Huffman coding
"
"------------------------------------------------"
nl := Character nl.
"------------------------------------------------"
" Referencing /bitstream.st"
Object subclass: BitStream [
   |bitIndex byte myStream|

   BitStream class >> new: fileStream [
      |result|

      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
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
]
"------------------------------------------------"
" Referencing /charfreq.st for Character object"
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]
" -> ZeroArray object"
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

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]
"------------------------------------------------"
" Referencing /outbits.st"
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
"------------------------------------------------"
" Referencing /tree.st to create leaf nodes & tree"
Object subclass: Leaf [
   |char count|

   char [ ^ char ]
   count [ ^ count ]
   "Add bool for leaf node"
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

   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]
   "Postorder traversal visitor value"
   postOrder: visitor [
      visitor value: char value: (0 bitAt: 1).
   ]

].
" . . . -> tree definition as subclass of leaf"
Leaf subclass: Tree [
   |left right|

   isLeaf [ ^ false ]
   getLeft [ ^ left ]
   getRight [ ^ right ]

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

   depthFirst: visitor prefix: string [
      left depthFirst: visitor prefix: string, '0'.
      right depthFirst: visitor prefix: string, '1'.
   ]
   "postOrder values -> traverse left-most first"
   postOrder: visitor [
      left postOrder: visitor.
      right postOrder: visitor.
      visitor value: char value: (1 bitAt: 1).
   ]

].
"------------------------------------------------"
" Main -> Entry point for program "
(Smalltalk arguments size < 2)
   ifTrue: [
      stderr << 'Error: Usage: hzip.st -d -t -c -u inputfile [outputfile]' << nl.
      ObjectMemory quit: 1.
   ].

|filein fileout|
debugFlag := false.
printTreeFlag := false.
compressFlag := false.
decompressFlag := false.
inputProvided := false.
outputProvided := false.

" Referencing Examples/cmdline.st ... Parse args and flags"
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
   (arg = nil) ifTrue: [
      opt = $d ifTrue: [debugFlag := true].
      opt = $t ifTrue: [printTreeFlag := true].
      opt = $c ifTrue: [compressFlag := true].
      opt = $u ifTrue: [decompressFlag := true].
   ]
   ifFalse: [
      inputProvided ifTrue: [
         outputProvided := true.
         fileout := arg.
      ] ifFalse: [
         inputProvided := true.
         filein := arg.
      ]
   ]
]
" Escape if some unexpected option input"
ifError: [
   stderr << 'Error: Usage: hzip.st -d -t -c -u inputfile [outputfile]' << nl.
   ObjectMemory quit: 1.
].

" Start debug output if flag is set"
debugFlag ifTrue: [
   stdout << 'DEBUGGER ACTIVE: ' << nl.
   stdout << 'INPUT FILE IS: ' << filein << nl.
   stdout << 'OUTPUT FILE IS: ' << fileout << nl.
].
"------------------------------------------------"
" Begin file compression -> "
" If -t or -c flag specified, compress the file accordingly"
(printTreeFlag | compressFlag) ifTrue: [

   "*****************************************************"
   "STEP 1 -> READ INPUT INTO FREQUENCY TABLE"
   "Open the input file for reading"
   inputData := FileStream open: filein mode: FileStream read.

   "Referencing /catfile.st"
   "Create a frequency table to count the number of times
    a character appears on input"
   frequencyTable := ZeroArray new: 257.
   frequencyTable incr: 256.
   [inputData atEnd not] whileTrue: [
      "Pass values into frequency table"
      idx := inputData next asInteger.
      frequencyTable incr: idx.
   ].
   "Close file descriptor"
   inputData close.
   "*****************************************************"

   "*****************************************************"
   "STEP 2 -> CREATE NODES FOR NON ZERO ELEMENTS"
   "Create priority queue"
   priorityQueue := SortedCollection new.
   "Create leaf nodes from values in frequency table
    and add them to the priority queue"
   debugFlag ifTrue: [stdout << 'FREQUENCY TABLE:' << nl.].
   frequencyTable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
          "If debugFlag is set, format data to stdout"
          debugFlag ifTrue: [
              stdout << 'freq[' << key asCharacter visible << '|' << key << ']=' << value << nl.
          ].
          "Create and add nodes"
          leafNode := Leaf new: key asCharacter count: value.
          priorityQueue add: leafNode.
      ]
   ].
   "*****************************************************"

   "*****************************************************"
   "STEP 3 -> REPEAT REMOVAL OF 2 SMALLEST ELEMENTS"
   "Repeatedly remove the two smallest elements from the
    priority queue until one tree is left"
   [priorityQueue size > 1] whileTrue: [
      "Pick two smallest nodes"
      firstNode := priorityQueue removeFirst.
      secondNode := priorityQueue removeFirst.
      "Calculate frequency from node value"
      totalFreq := firstNode count + secondNode count.
      "Construct tree and append to priority queue"
      tree := Tree new: firstNode char count: totalFreq
               left: firstNode right: secondNode.
      priorityQueue add: tree.
   ].
   "*****************************************************"

   "*****************************************************"
   "STEP 4 -> FOR ALL NON-ZERO VALUES, ENCODE STRING USING DFS"
   "For each character that has appeared as non-zero in the frequency
    table, construct an encoding string, using a DFS"
   encodingTable := ZeroArray new: 257.
   "Grab root node from queue"
   rootNode := priorityQueue removeFirst.

   "Output the tree if in debugger mode"
   debugFlag ifTrue: [stdout << 'DFS AFTER TREE EXTRACTION:' << nl.].
   rootNode depthFirst: [:char :string |
      "format data to stdout"
      debugFlag ifTrue: [stdout << '[' << char asCharacter visible << ' | '
                     << char asInteger << ']=' << string << nl.].
      encodingTable at: char asInteger put: string.
   ] prefix: ''.
   "*****************************************************"

   "*****************************************************"
   "STEP 5 -> IF -t FLAG IS SET, OUTPUT ENCODING TABLE SORTED BY CHAR"
   "If printTreeFlag flag is true"
   printTreeFlag ifTrue: [
      debugFlag ifTrue: [stdout << '-t FLAG OUTPUT:' << nl].
      "Traverse frequency table to get key"
      frequencyTable keysAndValuesDo: [:key :value |
         (value notNil and: [value > 0]) ifTrue: [
            path := encodingTable at: key.
            "Check is char key is printable"
            ((key > 32) & (key < 127)) ifTrue: [
               stdout << ' ' << key asCharacter << ' '.
            "If char is a hex"
            ] ifFalse: [
               size := (key printString: 16) size.
               (size = 1) ifTrue: [
                  stdout << 'x0' << (key printString: 16).
               ].
               (size = 3) ifTrue: [
                  stdout << 'EOF'.
               ].
               (size = 2) ifTrue: [
                  stdout << 'x' << (key printString: 16).
               ].
            ].
            "Pad bits to stdout"
            stdout << (value printPaddedWith: Character space to: 8)
                     << '  ' << path << nl.
         ]
      ]
   "*****************************************************"

   "*****************************************************"
   "STEP 6 -> IF -t FLAG IS NOT SET, OUTPUT ENCODING TABLE USING POSTORDER TRAVERSAL"
   "If printTreeFlag is NOT true"
   ] ifFalse: [
      |outBytes|
      "Open file for streaming"
      writeBitOut := [:outputFile|
         outBytes := OutBits new: outputFile.

         rootNode postOrder: [:char :bit |
            "If the node is a leaf"
            (bit = (0 bitAt: 1)) ifTrue: [
               "Write out of the leaf 0 bit"
               outBytes writeBit: (0 bitAt: 1).
               "If the byte is 0 stdout 0 bit then the 0 byte"
               char asInteger = 0 ifTrue: [
                  1 to: 9 do: [:idx | outBytes writeBit: (0 bitAt: 1)].
               ].
               "If EOF marker, output 1 bit then the 0 byte"
               char asInteger = 256 ifTrue: [
                  1 to: 8 do: [:idx | outBytes writeBit: (0 bitAt: 1)].
                  outBytes writeBit: (1 bitAt: 1)
               ].
               "Char is not 0 or EOF"
               ((char asInteger > 0) & (char asInteger < 256))
               ifTrue: [
               1 to: 8 do: [:idx |
                  outBytes writeBit: (char asInteger bitAt: (9 - idx))
               ].]
            "For interior nodes ... write out 1 Bit"
            ] ifFalse: [
               outBytes writeBit: (1 bitAt: 1).
            ].
         ].
         "Write another 1 bit to indicate the end of the tree"
         outBytes writeBit: (1 bitAt: 1).
   "*****************************************************"

   "*****************************************************"
   "STEP 7 -> REOPEN INPUT FILE, AND FOR EACH BYTE, WRITE THE ENCODED VERSION TO OUTPUT FILE"
         "Reopen input file again"
         inputData := FileStream open: filein mode: FileStream read.
         [inputData atEnd not] whileTrue: [
            "Get data from stream"
            key := inputData next asInteger.
            path := encodingTable at: key.
            1 to: path size do: [:idx |
               "Write encoded version"
               ascii := path byteAt: idx.
               ascii = 48 ifTrue: [outBytes writeBit: (0 bitAt: 1)].
               ascii = 49 ifTrue: [outBytes writeBit: (1 bitAt: 1)].
            ].
         ].
         "Write out the encoding string for EOF"
         eofPath := encodingTable at: 256.
         1 to: eofPath size do: [:idx |
            ascii := eofPath byteAt: idx.
            ascii = 48 ifTrue: [outBytes writeBit: (0 bitAt: 1)].
            ascii = 49 ifTrue: [outBytes writeBit: (1 bitAt: 1)].
         ].
        "flush bytes and close data stream"
         outBytes flushByte.
         inputData close.
      ].
      "If output file is provided write to that, else write stdout"
      outputProvided ifTrue: [
         outputFile := FileStream open: fileout mode: FileStream write.
         writeBitOut value: outputFile.
         outputFile close.
      ]
      ifFalse: [writeBitOut value: stdout.].
   "*****************************************************"

   "*****************************************************"
   "STEP 8 -> If the debug flag is set, write operations to debugger"
      debugFlag ifTrue: [
         stdout << 'DEBUG ENCODING OUTPUT:' << nl.
         stdout << 'FILEOUT:' << fileout << nl.

         "Print the encoded tree first"
         stdout << 'ENCODED TREE BELOW:' << nl.
         rootNode postOrder: [:char :bit |
            "If the node is a leaf"
            (bit = (0 bitAt: 1)) ifTrue: [
               stdout << bit.
               "first check for 0 and 256 values in node"
               char asInteger = 0 ifTrue: [stdout << '000000000'].
               char asInteger = 256 ifTrue: [stdout << '000000001'].
               "for all values between 0-256"
               ((char asInteger > 0) & (char asInteger < 256))
               ifTrue: [
                  1 to: 8 do: [:idx |
                    "print to stdout with offset"
                     stdout << (char asInteger bitAt: (9 - idx)).
                  ].
               ].
            "For interior nodes ... write out 1 Bit"
            ] ifFalse: [
               stdout << bit.
            ].
         ].
         "end of tree reached, write out the eof 1 bit"
         stdout << 'EOF Tree = ' << 1 << nl.

         "Print paths next"
         stdout << 'PATHS BELOW:' << nl.
         "read data input and get path from byte, then print path"
         inputData := FileStream open: filein mode: FileStream read.
         [inputData atEnd not] whileTrue: [
            byte := inputData next asInteger.
            path := encodingTable at: byte.
            stdout << path << ' '.
         ].
         "write out the eof encoded path"
         eofPath := encodingTable at: 256.
         stdout << eofPath << nl.
         "close input"
         inputData close.
      ].
   "*****************************************************"
   ].
]
"------------------------------------------------"
" Begin file decompression -> "
"if decompression flag is true"
(decompressFlag) ifTrue: [
   |byte endOfTree endOfDecode|

   "*****************************************************"
   "STEP 1 -> Open file and reconstruct Huffman decoding tree"
   inputData := FileStream open: filein mode: FileStream read.
   fileToBits := BitStream new: inputData.
   byteStack := OrderedCollection new.
   endOfTree := false.

   "Loop until endOfTree"
   [endOfTree] whileFalse: [
      firstBit := fileToBits nextBit.
      currBit := firstBit bitAt: 1.
      char := 0.
      "if curr bit is 0"
      currBit = 0 ifTrue: [
         "reconstruct the byte"
         1 to: 8 do: [:idx |
            bit := fileToBits nextBit.
            char := char bitAt: (9 - idx) put: bit.
         ].
         "if the byte is 0, read another bit"
         char = 0 ifTrue: [
            bit := fileToBits nextBit.
            bit = 1 ifTrue: [char := 256]
            ifFalse: [char := 0].
         ].
         "push byte to byteStack as a leaf node"
         leafNode := Leaf new: char asCharacter count: 1.
         byteStack addFirst: leafNode.
      "in this case currbit = 1"
      ] ifFalse: [
        "if stack's size is 1, we reached end of tree"
         byteStack size = 1 ifTrue: [
            endOfTree := true.
         ] ifFalse: [
            "otherwise pop two nodes from the stack"
            first := byteStack removeFirst.
            second := byteStack removeFirst.
            "add back as tree"
            tree := Tree new: 0 asCharacter count: 1
                              left: second right: first.
            byteStack addFirst: tree.
         ]
      ]
   ].
   "*****************************************************"

  "*****************************************************"
   "STEP 2 -> traverse tree to decode"
   "set decode done flag to false, and grab the tree from the stack"
   endOfDecode := false.
   decodedTree := byteStack removeFirst.
   "if there is a desired output file, send the output there
    otherwise print to stdout"
   outputProvided ifTrue: [
      outputFile := FileStream open: fileout mode: FileStream write.
      outBytes := OutBits new: outputFile.
   ] ifFalse: [
      outBytes := OutBits new: stdout.
   ].

   "loop until decode is complete"
   [endOfDecode] whileFalse: [
      |currPtr|
      "set current pointer to the tree we grabbed from the stack"
      currPtr := decodedTree.
      "while the curr tree is not a leaf"
      [currPtr isLeaf] whileFalse: [
         "traverse tree using bit values until you find a leaf"
         bit := fileToBits nextBit.
         bit = 0 ifTrue: [currPtr := currPtr getLeft].
         bit = 1 ifTrue: [currPtr := currPtr getRight].
      ].
      "if the curr node's value is 256 exit the decode"
      (currPtr char asInteger) = 256 ifTrue: [endOfDecode := true]
      "Write to stdout or file"
      ifFalse: [
        "get byte value from node"
         byte := (currPtr char) asInteger.
         1 to: 8 do: [:idx |
            outBytes writeBit: (byte bitAt: (9 - idx)).
         ].
      ]
   ].
   "close any output file when complete"
   outputProvided ifTrue: [outputFile close]
   "*****************************************************"
"------------------------------------------------"
]
