      *****************************************************************
      *    COPY-MEMBER: DRAS0024                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0024                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    28.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !02.04.09!    ! NEU                       ! Dürr     *
      * AAS0872 !01.02.10!    ! BES redefines 9(003) x 5  ! Dürr     *
      * AAS0889 !18.03.10!    ! Feld ARCHIV eingefügt     ! Dürr     *
      * ASL00315!05.07.10!    ! Tabelle in 2 Tab. geteilt ! Dürr     *
200922* VLA-5363!20.09.22!    ! Num. gepackte Felder      ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAS0024
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0024.
           05  PI-DRAS0024-VB-NR              PIC  S9(009).
161109     05  PI-DRAS0024-KNR-NP             PIC  9(003).
           05  PI-DRAS0024-DATUM-BIS          PIC  X(010).
           05  PI-DRAS0024-MONATE-NP          PIC  9(002).
           05  PI-DRAS0024-MONATE-UMSATZ      PIC  9(002).
           05  PI-DRAS0024-VARIANTE           PIC  9(001).
           05  PI-DRAS0024-GESELL             PIC  9(001).
010210*    05  PI-DRAS0024-BES                PIC  X(003).
010210*    05  PI-DRAS0024-BES-9 REDEFINES PI-DRAS0024-BES
      *                                       PIC  9(003).
010210     05  PI-DRAS0024-BES.
010210       07   FILLER OCCURS 10.
010210         10 PI-DRAS0024-BES-X           PIC  X(003).
010210         10 PI-DRAS0024-BES-N  REDEFINES PI-DRAS0024-BES-X
010210                                        PIC  9(003).
           05  PI-DRAS0024-MULTIPL            PIC  9(003).
180310     05  PI-DRAS0024-ARCHIV             PIC  X(001).
      *
       01  PO-DRAS0024.
           05  PO-DRAS0024-RC                 PIC  X(003).
               88 PO-DRAS0024-RC-OK                        VALUE 'OK '.
               88 PO-DRAS0024-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAS0024-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRAS0024-RC-ERR                       VALUE 'ERR'.
      *
200922     05  PO-DRAS0024-EH-DIR-1X          PIC  S9(010)V9 COMP-3.
200922     05  PO-DRAS0024-EH-DIR-NX          PIC  S9(010)V9 COMP-3.
200922     05  PO-DRAS0024-EH-INDIR-1X        PIC  S9(010)V9 COMP-3.
200922     05  PO-DRAS0024-EH-INDIR-NX        PIC  S9(010)V9 COMP-3.
200922     05  PO-DRAS0024-ANZ-DIR            PIC  S9(010)V9 COMP-3.
200922     05  PO-DRAS0024-ANZ-INDIR          PIC  S9(010)V9 COMP-3.
050710*    05  PO-DRAS0024-NP-TAB             PIC  X(140000).
050710*    05  PO-DRAS0024-NP-TAB-D           PIC  X(185000).
050710*    05  PO-DRAS0024-NP-TAB-I           PIC  X(185000).
080710     05  PO-DRAS0024-NP-TAB-D           PIC  X(265000).
080710     05  PO-DRAS0024-NP-TAB-I           PIC  X(265000).
      *
      ******* ENDE COPY-MEMBER **************************************
