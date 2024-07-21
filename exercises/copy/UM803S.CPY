      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UM803S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UM803S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:36:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    13.04.92 22:23:22
      *=================================================================
      * INFOCALL = FILELINK
      *            UM803S
      *-----------------------------------------------------------------
      *    F I L E L I N K                                    28.05.87
      *    U S E R - SCHNITTSTELLE FUER DAS EIN-/AUSLESEN VON HOSTFILES,
      *    EINZUFUEGEN IN INFOFILE (LINKAGE SECTION)
      *                   UM806010 (LINKAGE SECTION)
      *            UND IN INFOSERV (WORKING-STORAGE SECTION)
      *                   UM806    (WORKING-STORAGE SECTION)
      *            UND IN USER-PROGRAMME MIT INFOFILE-AUFRUF IN DIE
      *                WORKING-STORAGE SECTION NACH  01  FILELINK.
      *-----------------------------------------------------------------
           05  FL-FUNKTIONSCODE      PIC X.
             88  FL-DELETE                          VALUE 'D'.
             88  FL-SENDEN                          VALUE 'S'.
             88  FL-EMPFANGEN                       VALUE 'E'.
             88  FL-MOVE-RENAME                     VALUE 'M'.
             88  FL-MOVE-BATCH                      VALUE 'B'.
             88  FL-UNLOCK-FREIGEBEN                VALUE 'U'.
             88  FL-READ-DIRECTORY                  VALUE 'R'.
             88  FL-FREE-AND-DELETE                 VALUE 'F'.
             88  FL-AUSTAUSCH-ZEITKENNUNG           VALUE 'T'.
VSAMB        88  FL-CLOSE-INFOPOOL                  VALUE 'Z'.
VSAMB        88  FL-DATUM-ZEIT                      VALUE 'Y'.
           05  FL-RETURNCODE         PIC X.
             88  FL-OK               VALUE ' '.
             88  FL-I-O-FEHLER       VALUE '1'.
             88  FL-PARAM-FEHLER     VALUE '2'.
             88  FL-DE-NICHT-DA      VALUE '4'.
             88  FL-DATAPOOL-VOLL    VALUE '5'.
             88  FL-POSITION-ZU-HOCH VALUE '6'.
             88  FL-DATEI-GESPERRT   VALUE '7'.
             88  FL-DIRECTORY-VOLL   VALUE '8'.
             88  FL-DATEI-VORHANDEN  VALUE '9'.
           05  FL-FILE-IDENT.
             10  FL-LIB              PIC X.
               88  FL-DEFAULT-PRIVAT                VALUE '1'.
               88  FL-PUBLIC                        VALUE '2'.
             10  FL-PUBLIC-ID        PIC X(8).
             10  FL-NAMEN.
                15  FL-PFAD          PIC X(64).
                15  FL-NAME          PIC X(8).
                15  FL-SUFFIX        PIC X(3).
             10  FL-OWNER-ID         PIC X(8).
           05  FL-FILE-IDENT-NEU.
             10  FL-LIB-NEU          PIC X.
               88  FL-DEFAULT-PRIVAT-NEU            VALUE '1'.
               88  FL-PUBLIC-NEU                    VALUE '2'.
             10  FL-PUBLIC-ID-NEU    PIC X(8).
             10  FL-NAMEN-NEU.
                15  FL-PFAD-NEU      PIC X(64).
                15  FL-NAME-NEU      PIC X(8).
                15  FL-SUFFIX-NEU    PIC X(3).
           05  FL-OWNER-ID-NEU       PIC X(8).
           05  FL-CODE               PIC X.
      *             GUELTIG SIND NUR GROSSE BUCHSTABEN
             88  FL-CODE-DEFAULT-EBCDIC             VALUE 'E'.
             88  FL-CODE-TRANSPARENT                VALUE 'T'.
           05  FL-FORMAT           PIC X.
      *             GUELTIG SIND NUR GROSSE BUCHSTABEN
             88  FL-FORMAT-DEFAULT-DISPLAY          VALUE 'D'.
             88  FL-FORMAT-STRING                   VALUE 'S'.
             88  FL-FORMAT-TEXT                     VALUE 'T'.
           05  FL-FILE-RESTLAENGE    PIC 9(8).
           05  FL-ALLOCLAENGE        PIC 9(8).
           05  FL-SUBFDENR           PIC 9(5).
           05  FL-AB-POSITION        PIC 9(8).
           05  FL-DATENLAENGE        PIC 9(5).
           05  FL-DATENBEREICH.
==>          10  FL-DATENBYTE        PIC X          OCCURS 32000.
           05  FL-DIRECTORY          REDEFINES FL-DATENBEREICH.
      *          FL-DE-OCCURS-ANGABE = LAENGE VON FL-DATENBEREICH / 160
==>          10  FL-DE                   OCCURS 200 TIMES.
               15  FL-DENR                      PIC  9(0005).
               15  FL-DEIDENT.
                 20  FL-DE-LIB                  PIC  X(0001).
                 20  FL-DE-PUBLIC-ID            PIC  X(0008).
                 20  FL-DE-NAMEN.
                   25  FL-DE-NAME               PIC  X(0008).
                   25  FL-DE-SUFFIX             PIC  X(0003).
                   25  FL-DE-KEY-ERG-LAENGE     PIC  9(0002).
                   25  FL-DE-LFDNR              PIC  9(0004).
               15  FILLER                       PIC  X(0001).
               15  FL-DE-MANAGEMENT-FELDER.
                 20  FL-DE-OWNER-ID             PIC  X(0008).
                 20  FL-DE-CODE                 PIC  X(0001).
                 20  FL-DE-FORMAT               PIC  X(0001).
                 20  FL-DE-STATUS               PIC  X(0001).
                 20  FL-DE-LOGONZAEHLER         PIC  9(0005).
                 20  FL-DE-FILELAENGE           PIC  9(0008).
                 20  FL-DE-ALLOCLAENGE          PIC  9(0008).
                 20  FL-DE-LAST-UPDATE.
                   25  FL-DE-DATUM-LAST-UPDATE.
                     30  FL-DE-JAHR-LU          PIC  9(0002).
                     30  FL-DE-MONAT-LU         PIC  9(0002).
                     30  FL-DE-TAG-LU           PIC  9(0002).
                   25  FL-DE-ZEIT-LU.
                     30  FL-DE-STUNDE-LU        PIC  9(0002).
                     30  FL-DE-MINUTE-LU        PIC  9(0002).
                     30  FL-DE-SECUNDE-LU       PIC  9(0002).
                     30  FL-DE-HSEC-LU          PIC  9(0002).
                 20  FL-DE-LAST-READ.
                   25  FL-DE-DATUM-LAST-READ.
                     30  FL-DE-JAHR-LR          PIC  9(0002).
                     30  FL-DE-MONAT-LR         PIC  9(0002).
                     30  FL-DE-TAG-LR           PIC  9(0002).
                   25  FL-DE-ZEIT-LR.
                     30  FL-DE-STUNDE-LR        PIC  9(0002).
                     30  FL-DE-MINUTE-LR        PIC  9(0002).
                     30  FL-DE-SECUNDE-LR       PIC  9(0002).
                     30  FL-DE-HSEC-LR          PIC  9(0002).
               15  FL-DE-KEY-ERG                PIC  X(0064).
               15  FILLER                       PIC  X(0004).
           05  FILLER    REDEFINES     FL-DATENBEREICH.
             10  FL-INFOPOOL-BELEGUNG.
               15  FL-AUANZAHL                  PIC  9(0008).
               15  FL-DEANZAHL                  PIC  9(0008).
               15  FL-AULAENGE                  PIC  9(0004).
               15  FL-DELAENGE                  PIC  9(0004).
               15  FL-AUBELEGT                  PIC  9(0008).
               15  FL-DEBELEGT                  PIC  9(0008).
               15  FL-VERSION-X                 PIC  9(0004).
==>          10  FILLER                         PIC  X(31956).

