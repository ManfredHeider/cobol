      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0527.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0527.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:02:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0527.
000020*    Ausgabebereich für UPRO A0000015
000030        07 JOBNAME                PICTURE X(8).
000040*    Teil eines DV-technischen Jobs
000050        07 JOBNAME-R REDEFINES JOBNAME.
000060*    Redefiniton des Jobnamens
000070           09 JOB-HV              PICTURE X.
000080*    Filler-1
000090           09 FILLER              PICTURE X(7).
000100*    Filler-7
000110        07 PROCNAME               PICTURE X(8).
000120*    Teil eines DV-technischen Jobs
000130        07 STEPNAME               PICTURE X(8).
000140*    Teil eines DV-technischen Jobs
