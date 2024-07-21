      ***************************************************************** 00012000
      *    COPY-MEMBER: DRPA0003                                      *
      *    --------------------------------------------------------   *
      *    TABELLE FüR UMSETZUNG KOMPASS-FEHLERCODES IN DVAG-F.CODES  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. MUELLER                                       * 00060000
      *    DATUM:    16.01.1996                                       * 00070000
      ****************************************************************  00091000
      *    VERSION
      ****************************************************************  00091200
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      *         !        !    ! NEU                       ! MUELLER  *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *                                                              *  00099400
      * ENDE-LOGBUCH                                                 *  00099500
      ****************************************************************  00099600
      *
      *   BESCHREIBUNG DER TABELLE:
      *    - FEHLERNUMMER IN KOMPASS
      *    -
      *    - FEHLERNUMMER IN DAISY, ENTSPRICHT DVAG-STANDARDS
      *    - ORIGINAL FEHLERTEXT AUS KOMPASS
      *
      ****************************************************************  00099600

       01  DRPA0003.
           05  FILLER  PIC X(08) VALUE
            '01100004'.
           05  FILLER  PIC X(08) VALUE
            '        '.
           05  FILLER  PIC X(05) VALUE
            '     '.
           05  FILLER  PIC X(70) VALUE
            'Programmfehler bei DB2-Zugriff # $                       '.
      *
           05  FILLER  PIC X(08) VALUE
            '01100011'.
           05  FILLER  PIC X(08) VALUE
            '        '.
           05  FILLER  PIC X(05) VALUE
            '     '.
           05  FILLER  PIC X(70) VALUE
            'Objektidentifikation ungültig                            '.
      *
           05  FILLER  PIC X(08) VALUE
            '01100018'.
           05  FILLER  PIC X(08) VALUE
            '        '.
           05  FILLER  PIC X(05) VALUE
            '     '.
           05  FILLER  PIC X(70) VALUE
            'Beziehung zu Fremdschlüssel fehlerhaft                   '.
      *
           05  FILLER  PIC X(08) VALUE
            '01100020'.
           05  FILLER  PIC X(08) VALUE
            'UPPA0106'.
           05  FILLER  PIC X(05) VALUE
            'E0103'.
           05  FILLER  PIC X(70) VALUE
            '                                                         '.
      *
           05  FILLER  PIC X(08) VALUE
            '01100024'.
           05  FILLER  PIC X(08) VALUE
            '        '.
           05  FILLER  PIC X(05) VALUE
            '     '.
           05  FILLER  PIC X(70) VALUE
            'Operation ungültig                                       '.
      *
           05  FILLER  PIC X(08) VALUE
            '01110009'.
           05  FILLER  PIC X(08) VALUE
            'UPPA0104'.
           05  FILLER  PIC X(05) VALUE
            'E0101'.
           05  FILLER  PIC X(70) VALUE
            '                                                         '.
      *
           05  FILLER  PIC X(08) VALUE
            '01110009'.
           05  FILLER  PIC X(08) VALUE
            'UPPA0105'.
           05  FILLER  PIC X(05) VALUE
            'E0102'.
           05  FILLER  PIC X(70) VALUE
            '                                                         '.
      *
       01  DRPA0003-RED REDEFINES DRPA0003.
           03  DRPA0003-SATZ OCCURS 007 indexed by z-err-ind.
               05  DRPA0003-NR-KOMPAS    PIC X(08).
               05  DRPA0003-PG-ID        PIC X(08).
               05  DRPA0003-NR-DAISY     PIC X(05).
               05  DRPA0003-TEXT         PIC X(70).
