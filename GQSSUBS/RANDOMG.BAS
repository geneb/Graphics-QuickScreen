'****************************************************************************
'Random Access File Input and Output Routines.
'
'Copyright (c) 1991, Crescent Software, Inc.
'by Don Malin
'
'This module contains routines for random access file I/O with GQS
'data entry forms.  In addition to supporting regular random access files,
'these routines also save and retrieve multi-line notes fields to a linked
'list data file.  In order to access these routines, load this file as a
'module.
'
'For every Notes field on a form, 4 bytes are reserved in the random access
'file (.DAT) for a lomg integer byte offset pointer to the Notes data in the
'seperate Notes database file (.NOT).  The first 2 bytes that are pointed to
'contain the length of the Notes data as follows:
'
'   Random Access Record (.DAT)             Note Database (.NOT)
'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄ¿        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
'³Record #1 Data         ³Ptr=1 ÃÄÄÄÄÄÄÄ>³23"Note data for record #1"
'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄ´   ÚÄÄÄ>³47"Note data for record #2 which
'³Record #2 Data         ³Ptr=24ÃÄÄÄÙ    ³may be any length."
'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄ´    ÚÄÄ>³23"Note data for record #3"
'³Record #3 Data         ³Ptr=71ÃÄÄÄÄÙ   ³           .      .
'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄ´        ³           .      .           
'³      .       .        ³ . .  ³
'³      .       .        ³ . .  ³
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÙ
'
'When an existing Note record is shortened, it is saved in the same place in
'the file, and its length value is adjusted.  When a note other than the
'last one in the file is extended, its original data is abandoned and the
'new data is appended to the end of the file.  Space that is abandoned cannot
'be reclaimed during normal processing, so we have provided a utility program
'PACKNOTE.BAS that can rebuild a Note file after it has become fragmented.
'****************************************************************************
DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'                      'FieldInfo type and constants

'****************************************************************************
'This procedure retrieves a specified record from its database, as well as
'any notes that are associated with the record from a seperate Notes file
'(.NOT).
'
'Input:
'  RecNo& - is the record number to retrieve.
'  Form$() - is a "Forms" data array that has been properly initialized.
'  Fld() - is a Field Definition array.
'
'Output:
'  This routine loads the specified record into the Form$(0, 0) record
'  buffer array element.  Note that you must use "UnPackBuffer" in order to
'  fill in the remaining individual elements of Form$() before displaying
'  the form.
'****************************************************************************
'
SUB GetRecG (RecNo&, Form$(), Fld() AS FieldInfoG) STATIC

   GET Fld(0).RelHandle, RecNo&                 'Retrieve the main record

   FOR N = 1 TO Fld(0).Fields                   'Check for Note fields
      IF Fld(N).FType = NotesFld THEN           'If we have one,
                                                '  save its note file offset.
         NoteOffset& = CVL(MID$(Form$(0, 0), Fld(N).Fields, 4))
                                                
         IF NoteOffset& THEN                    'If there is an offset,
                                                'Read the note's length
            GET Fld(0).ScratchI, NoteOffset&, NoteLen
                                                'Pad the form element out to
            Form$(N, 0) = SPACE$(NoteLen)       '  the indicated length.
            GET Fld(0).ScratchI, , Form$(N, 0)  'Read the actual text
         ELSE                                   'No offset means there is no
            Form$(N, 0) = ""                    '  text, so null out the field.
         END IF
         EXIT FOR
      END IF
   NEXT

END SUB

'****************************************************************************
'This procedure opens a random access database (.DAT), and fields it to the
'data buffer - Form$(0, 0).  If there are multi-line notes fields contained
'in the form, a Notes database (.NOT) is also opened.
'
'Input:
'  FormName$ - is the base name of the databases to open.  A path may be
'              included, but the extension must be omitted.
'  Form$() - is a "Forms" data array that will hold the data.
'  Fld() - is a Field Definition array.
'
'Output:
'  If the specified databases (.DAT and .NOT) do not exist, they are created.
'  Form$(0, 0) is fielded to the random access file (.DAT)
'  Fld(0).RelHandle contains the handle associated with the .DAT file and
'  Fld(0).ScratchI holds the Notes database (.NOT) file number.
'****************************************************************************
'
SUB OpenFilesG (FormName$, Form$(), Fld() AS FieldInfoG)

    Fld(0).RelHandle = FREEFILE

    '----- Open and field the main database
    OPEN FormName$ + ".DAT" FOR RANDOM AS Fld(0).RelHandle LEN = Fld(0).StorLen
    FIELD Fld(0).RelHandle, Fld(0).StorLen AS Form$(0, 0)

    '----- Open all notes files
    FOR N = 1 TO Fld(0).Fields
       IF Fld(N).FType = NotesFld THEN
          Fld(0).ScratchI = FREEFILE

          OPEN FormName$ + ".NOT" FOR BINARY AS Fld(0).ScratchI
          EXIT FOR
       END IF
    NEXT

END SUB

'****************************************************************************
'This procedure saves data from a form to a specified record in its database.
'In addition, the multi-line text field's data is save to an associated
'"Notes Database" (.NOT).
'
'Input:
'  RecNo& - is the record number to save to.
'  Form$() - is a "Forms" data array that contains the data to be saved.
'  Fld() - is a Field Definition array.
'
'Output:
'  The data contained in Form$(0, 0) is saved to the random access file (.DAT)
'  Notes contained in Form$(N, 0) are saved to the Notes Database (.NOT).
'****************************************************************************
'
SUB SaveRecG (RecNo&, Form$(), Fld() AS FieldInfoG) STATIC

   LastRec& = LOF(Fld(0).RelHandle) \ Fld(0).StorLen

   '----- Check for and save Note fields to their files
   FOR N = 1 TO Fld(0).Fields
      IF Fld(N).FType = NotesFld THEN           'Is this a note field?

         NoteLen = LEN(Form$(N, 0))             'Save its length
         
         IF NoteLen THEN                        'Is there something to save?
            IF RecNo& <= LastRec& THEN          'Is this an existing record?
                                                'Yes, save previous offset
               NoteOffset& = CVL(MID$(Form$(0, 0), Fld(N).Fields, 4))

               IF NoteOffset& THEN              'If there was an offset,
                                                '  read its length
                  GET Fld(0).ScratchI, NoteOffset&, PrevLen
                                                'If it's too short, make it 0
                  IF NoteLen > PrevLen THEN
                     IF NoteOffset& + PrevLen + 1 < LOF(Fld(0).ScratchI) THEN
                        NoteOffset& = 0
                     END IF
                  END IF
               END IF
            ELSE                                'New record, set offset to 0
               NoteOffset& = 0                  '  for later tests
            END IF

            IF NoteOffset& = 0 THEN             'If offset is 0, make it the
                                                '  end of the file.
               NoteOffset& = LOF(Fld(0).ScratchI) + 1
            END IF
                                                'Save the note's length
            PUT Fld(0).ScratchI, NoteOffset&, NoteLen
            PUT Fld(0).ScratchI, , Form$(N, 0)  'Save the note's text
         ELSE
            NoteOffset& = 0                     'Nothing to save so set to 0
         END IF
                                                'Save note's offset in buffer
         MID$(Form$(0, 0), Fld(N).Fields) = MKL$(NoteOffset&)

         EXIT FOR
      END IF
   NEXT

   PUT Fld(0).RelHandle, RecNo&                 'Save the form's record

END SUB

