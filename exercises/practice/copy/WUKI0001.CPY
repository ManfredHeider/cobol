      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   Wuki0001.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/MF/CPY/Wuki0001.CPv  $
      *
      *   Rev 2.6   23 Oct 2001 16:10:06   t13ex48
      *AKI00216
      *Aufbau Test-DB(RP und XDB)
      *
      *   Rev 2.5   25 Jul 2001 10:32:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * :EA: für flexible Verwendung des Copys (REPLACING)

        05  :EA-:DCL-ADRESSART.
           10 :EA-:ADRESSARTID               PIC S9(4) COMP.
           10 :EA-:ADRESSARTKZ               PIC X(01).
           10 :EA-:ADRESSARTTEXT             PIC X(30).
      ******************************************************************
