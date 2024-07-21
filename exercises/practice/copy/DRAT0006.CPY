      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 16:53:26   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Nov 04 1995 08:34:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0006                                      *
      *    --------------------------------------------------------   *
      *    RAHMEN: DRUCKAUFBEREITUNG ERROR-BEREICH                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER (INTEGRATA AG)                      *
      *    DATUM:    27.11.1992                                       *
      *****************************************************************
      *    VERSION   3    10.03.95 15:57:39
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !13.04.93! 01 ! NEU                       ! SCHÖLLH. *
      * AFVB0144!19.08.94!    ! ZEILEN (FILLER) BENANNT,  ! SCHÖLLH. *
      *         !        !    ! REDEF. AUF D-ERR-MELD-TEXT!          *
      *         !06.09.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * FEVB0156!27.09.94!    ! FELDER DATUM, UHRZEIT NEU ! SCHÖLLH. *
      *         !28.09.94! 03 ! PROD.ÜBERGABE             ! SCHÖLLH. *
      * FEVB0218!03.03.95!    ! TRANS- UND MAP-FELDER     ! SCHÖLLH. *
      *         !        !    !  + UMGESTALTUNG           !          *
      *         !        !    !  + ALTE LABEL ENTFERNT    !          *
      *         !10.03.95! 04 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFPA0012!06.11.97! 05 ! SCHLüSSEL DES AKTUELL     ! AUERBACH *
      *         !        !    ! BEARBEITETEN EINGABESATZ  !          *
      *         !        !    ! (NUR FüR BATCH-RAHMEN)    !          *
      *         !12.08.98!    ! CICS-NAME UND USER-ID     ! AUERBACH *  00070500
      *         !        !    ! IM ERRIPRO-BILD ERGäNZT   !          *  00010000
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DRUCKAUFBEREITETE ERROR-AUSGABE FUER AUSGABE AUF DEN
      *    BILDSCHIRM
      *
       01  D-ERROR-BEREICH.

           05  D-ERR-ZEILE-01.
               10 D-ERR-DATUM            PIC  X(010).
               10 FILLER                 PIC  X(001) VALUE SPACE.
               10 D-ERR-ZEIT             PIC  X(005).
120898*        10 FILLER                 PIC  X(024) VALUE
120898*                                    ' ------   FEHLER AUFGETR'.
120898*        10 FILLER                 PIC  X(040) VALUE
120898*                    'ETEN  -  FEHLER-ANALYSE   --------------'.

120898         10 FILLER                 PIC  X(021) VALUE
120898                                     ' ---   FEHLER AUFGETR'.
120898         10 FILLER                 PIC  X(030) VALUE
120898                     'ETEN  -  FEHLER-ANALYSE   --- '.
120898         10 D-ERR-CICS-NAME        PIC  X(004).
120898         10 FILLER                 PIC  X(001) VALUE SPACE.
120898         10 D-ERR-USERID           PIC  X(008).

           05  D-ERR-ZEILE-02.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.

           05  D-ERR-ZEILE-03.
               10 FILLER                 PIC  X(040) VALUE
                           'FEHLER-TYP                            : '.
               10 D-ERR-FEHLER-TYP-TEXT  PIC  X(040).

030395*     05  D-ERR-ZEILE-04.
030395*         10 FILLER                 PIC  X(040) VALUE
030395*                     'PROGRAMM-NAME / UPRO-NAME             : '.
030395*         10 D-ERR-PROGNAME         PIC  X(008).
030395*         10 FILLER                 PIC  X(003) VALUE ' / '.
030395*         10 D-ERR-UPRONAME         PIC  X(008).
030395*         10 FILLER                 PIC  X(021).
030395*
030395*     05  D-ERR-ZEILE-05.
030395*         10 FILLER                 PIC  X(040) VALUE
030395*                     'AUFGETRETEN IN (SECTION, LFD-NR)      : '.
030395*         10 D-ERR-ORT-SEC          PIC  X(005).
030395*         10 FILLER                 PIC  X(001).
030395*         10 D-ERR-ORT-LFD          PIC  9(003).
030395*         10 FILLER                 PIC  X(031).

030395     05  D-ERR-ZEILE-04.
030395         10 FILLER                 PIC  X(040) VALUE
030395                     'PROGRAMM / UPRO / LFD-NR              : '.
030395         10 D-ERR-PROGNAME         PIC  X(008).
030395         10 FILLER                 PIC  X(003) VALUE ' / '.
030395         10 D-ERR-UPRONAME         PIC  X(008).
030395         10 FILLER                 PIC  X(003) VALUE ' / '.
030395         10 D-ERR-ORT-LFD          PIC  9(003).
030395         10 FILLER                 PIC  X(001).
030395         10 D-ERR-ORT-SEC          PIC  X(005).
030395         10 FILLER                 PIC  X(009).
030395
030395     05  D-ERR-ZEILE-05.
030395         10 FILLER                 PIC  X(040) VALUE
030395                     'DIALOG / TEILDIALOG / MAPS            : '.
030395         10 D-ERR-DIALOG           PIC  X(004).
030395         10 FILLER                 PIC  X(003) VALUE ' / '.
C30395         10 D-ERR-DIALOG-SCHRITT   PIC  X(004).
030395         10 FILLER                 PIC  X(003) VALUE ' / '.
C30395         10 D-ERR-BILDER-TAB       PIC  X(021).
030395         10 FILLER                 PIC  X(005).

           05  D-ERR-ZEILE-06.
               10 FILLER                 PIC  X(040) VALUE
                           'SQL-CODE / CICS-RC / FILE-STATUS      : '.
               10 D-ERR-SQLCODE          PIC  -(009)9.
               10 FILLER                 PIC  X(003) VALUE ' / '.
               10 D-ERR-CICS-RC          PIC  -(004)9.
               10 FILLER                 PIC  X(003) VALUE ' / '.
               10 D-ERR-FILE-STATUS      PIC  X(002).
               10 FILLER                 PIC  X(017).



           05  D-ERR-ZEILE-07.
               10 FILLER                 PIC  X(010) VALUE
                           'SQLERRMC: '.
               10 D-ERR-SQLERRMC         PIC  X(070).

           05  D-ERR-ZEILE-08.
               10 FILLER                 PIC  X(010) VALUE
                           'HINWEIS:  '.
               10 D-ERR-MELD-TEXT        PIC  X(070).

      *       TRACE-ZEILE: 3 ZEILEN AUSGABE
           05  D-ERR-ZEILEN-09-11.
               10 D-ERR-TRACE-ZEILE.
                  15 FILLER              PIC  X(010) VALUE
                          'TRACE  :  '.
                  15 D-ERR-TRACE         PIC  X(230).
190894     05  FILLER REDEFINES D-ERR-ZEILEN-09-11.
190894         10 D-ERR-ZEILE-09         PIC  X(080).
190894         10 D-ERR-ZEILE-10         PIC  X(080).
190894         10 D-ERR-ZEILE-11         PIC  X(080).

           05  D-ERR-ZEILE-12.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.

      *       VAR-RAHMEN-BEREICH: 4 ZEILEN AUSGABE
           05  D-ERR-ZEILEN-13-16.
               10 D-ERR-VAR-RAHMEN       PIC  X(320).
190894     05  FILLER REDEFINES D-ERR-ZEILEN-13-16.
190894         10 D-ERR-ZEILE-13         PIC  X(080).
190894         10 D-ERR-ZEILE-14         PIC  X(080).
190894         10 D-ERR-ZEILE-15         PIC  X(080).
190894         10 D-ERR-ZEILE-16         PIC  X(080).

      *       VAR-BEREICH: 1 ZEILE KOMMENTAR; 4 ZEILEN AUSGABE
           05  D-ERR-ZEILEN-17-21.
               10 FILLER                 PIC  X(045) VALUE
                      'VARIABLER TRACE-BEREICH (AB NAECHSTE ZEILE) :'.
               10 FILLER                 PIC  X(035).

               10 D-ERR-VAR              PIC  X(320).
190894     05  FILLER REDEFINES D-ERR-ZEILEN-17-21.
190894         10 D-ERR-ZEILE-17         PIC  X(080).
190894         10 D-ERR-ZEILE-18         PIC  X(080).
190894         10 D-ERR-ZEILE-19         PIC  X(080).
190894         10 D-ERR-ZEILE-20         PIC  X(080).
190894         10 D-ERR-ZEILE-21         PIC  X(080).

           05  D-ERR-ZEILE-22.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.

           05  D-ERR-ZEILE-23.
               10 FILLER                 PIC  X(040) VALUE
                           '---            BITTE HARDCOPY MACHEN UND'.
               10 FILLER                 PIC  X(040) VALUE
                           ' DV-ABTEILUNG INFORMIEREN            ---'.

           05  D-ERR-ZEILE-24.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.
               10 FILLER                 PIC  X(040) VALUE
                           '----------------------------------------'.
      *

061197*    DRUCKAUFBEREITETE ERROR-AUSGABE SPEZIELL FüR DEN
061197*    BATCH-RAHMEN (NICHT FüR DIALOG ! )
061197*    HIER WIRD DER SCHLüSSEL DES AKTUELL BEARBEITETEN
061197*    EINGABESATZES ABGELEGT. DIES SOLL EIN SCHNELLES UND
061197*    SICHERES AUFFINDEN DES PROBLEMSATZES ERMöGLICHEN.
061197*
061197 01  D-ERROR-BEREICH-2.
061197
061197*       SCHLüSSEL-BEREICH: 1 ZEILE KOMMENTAR; 4 ZEILEN AUSGABE
061197     05  D-ERR-ZEILEN-25-29.
061197         10 FILLER                 PIC  X(040) VALUE
061197                     'SCHLUESSEL DES BEARBEITETEN EINGABESATZ '.
061197         10 FILLER                 PIC  X(040) VALUE
061197                     '(AB NAECHSTE ZEILE) :                   '.
061197
061197         10 D-ERR-SCHLUESSEL       PIC  X(320) VALUE SPACE.
061197     05  FILLER REDEFINES D-ERR-ZEILEN-25-29.
061197         10 D-ERR-ZEILE-25         PIC  X(080).
061197         10 D-ERR-ZEILE-26         PIC  X(080).
061197         10 D-ERR-ZEILE-27         PIC  X(080).
061197         10 D-ERR-ZEILE-28         PIC  X(080).
061197         10 D-ERR-ZEILE-29         PIC  X(080).

      ******** ENDE COPY-MEMBER D-ERROR-BEREICH (DRAT0006) ************

