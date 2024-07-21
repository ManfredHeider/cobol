      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0030.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DBPA0030.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:26:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 08:58:52   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   22 Jun 1998 08:53:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *Micro Focus BMS SCREENS Generated Code
      * MapSet Name : BPE0001
      * Date Created: 10/02/98
      * Time Created: 09:31:49
      * Alignment   : OFF

      *Input Data For Map BPA0030
       01 BPA0030I.
        03 FILLER                             PIC X(12).
        03 EGESL                              PIC S9(4) COMP.
        03 EGESF                              PIC X.
        03 EGESA REDEFINES EGESF PIC X.
        03 FILLER                             PIC X(001).
        03 EGESI                              PIC X(002).
        03 AGESBML                            PIC S9(4) COMP.
        03 AGESBMF                            PIC X.
        03 AGESBMA REDEFINES AGESBMF PIC X.
        03 FILLER                             PIC X(001).
        03 AGESBMI                            PIC X(010).
        03 EDTAARTL                           PIC S9(4) COMP.
        03 EDTAARTF                           PIC X.
        03 EDTAARTA REDEFINES EDTAARTF PIC X.
        03 FILLER                             PIC X(001).
        03 EDTAARTI                           PIC X(003).
        03 ADARTBML                           PIC S9(4) COMP.
        03 ADARTBMF                           PIC X.
        03 ADARTBMA REDEFINES ADARTBMF PIC X.
        03 FILLER                             PIC X(001).
        03 ADARTBMI                           PIC X(010).
        03 AFREIGAL                           PIC S9(4) COMP.
        03 AFREIGAF                           PIC X.
        03 AFREIGAA REDEFINES AFREIGAF PIC X.
        03 FILLER                             PIC X(001).
        03 AFREIGAI                           PIC X(001).
        03 EFREIGAL                           PIC S9(4) COMP.
        03 EFREIGAF                           PIC X.
        03 EFREIGAA REDEFINES EFREIGAF PIC X.
        03 FILLER                             PIC X(001).
        03 EFREIGAI                           PIC X(001).
        03 AANZFEHL                           PIC S9(4) COMP.
        03 AANZFEHF                           PIC X.
        03 AANZFEHA REDEFINES AANZFEHF PIC X.
        03 FILLER                             PIC X(001).
        03 AANZFEHI                           PIC X(009).
        03 EANZFEHL                           PIC S9(4) COMP.
        03 EANZFEHF                           PIC X.
        03 EANZFEHA REDEFINES EANZFEHF PIC X.
        03 FILLER                             PIC X(001).
        03 EANZFEHI                           PIC X(009).
        03 APSATZL                            PIC S9(4) COMP.
        03 APSATZF                            PIC X.
        03 APSATZA REDEFINES APSATZF PIC X.
        03 FILLER                             PIC X(001).
        03 APSATZI                            PIC X(005).
        03 EPSATZL                            PIC S9(4) COMP.
        03 EPSATZF                            PIC X.
        03 EPSATZA REDEFINES EPSATZF PIC X.
        03 FILLER                             PIC X(001).
        03 EPSATZI                            PIC X(005).

      *Output Data For Map BPA0030
       01 BPA0030O REDEFINES BPA0030I.
        03 FILLER                             PIC X(12).
        03 FILLER                             PIC X(3).
        03 EGESH                              PIC X.
        03 EGESO                              PIC X(002).
        03 FILLER                             PIC X(3).
        03 AGESBMH                            PIC X.
        03 AGESBMO                            PIC X(010).
        03 FILLER                             PIC X(3).
        03 EDTAARTH                           PIC X.
        03 EDTAARTO                           PIC X(003).
        03 FILLER                             PIC X(3).
        03 ADARTBMH                           PIC X.
        03 ADARTBMO                           PIC X(010).
        03 FILLER                             PIC X(3).
        03 AFREIGAH                           PIC X.
        03 AFREIGAO                           PIC X(001).
        03 FILLER                             PIC X(3).
        03 EFREIGAH                           PIC X.
        03 EFREIGAO                           PIC X(001).
        03 FILLER                             PIC X(3).
        03 AANZFEHH                           PIC X.
        03 AANZFEHO                           PIC X(009).
        03 FILLER                             PIC X(3).
        03 EANZFEHH                           PIC X.
        03 EANZFEHO                           PIC X(009).
        03 FILLER                             PIC X(3).
        03 APSATZH                            PIC X.
        03 APSATZO                            PIC X(005).
        03 FILLER                             PIC X(3).
        03 EPSATZH                            PIC X.
        03 EPSATZO                            PIC X(005).
