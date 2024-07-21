      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0525.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0525.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:01:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010 01  ERRORBEREICH.
000020*    ERRORBEREICH
000030     03 PROGRAMMNAME              PICTURE X(8).
000040*    PROGRAMMNAME
000050     03 VIEWNAME                  PICTURE X(18).
000060*    VIEWNAME
000070     03 OPERATION                 PICTURE XXX.
000080*    OPERATION
000090     03 SQL-RC                    PICTURE S9(6)  COMPUTATIONAL.
000100*    SQL-RC
000110     03 MSG-NR                    PICTURE 9(4).
000120*    MSG-NR
000130     03 RC                        PICTURE 99.
000140*    RC Returncode aus UPRO
