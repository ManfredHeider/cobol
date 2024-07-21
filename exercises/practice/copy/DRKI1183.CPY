      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1183.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1183.CPv  $
      *
      *   Rev 2.1   05 Apr 2001 10:20:32   T13AE44
      * 
      *
      *   Rev 2.0   17 Jan 2001 12:57:46   p13sp16
      *Archiverstellung
      *
      *
      *----------------------------------------------------------------*
      *
      *                                                                *
      *    LAENGE =  31 Bytes       ERSTELLT AM 13.03.01.              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      * AKI174 ! Neu                                   !Biehl    !01   *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1183.
         05 DRKI1183-DATEN.
      *    BEZ = MIS Bezeichnung
           10  BEZ             PIC X(8).
           10  FILLER          PIC X.
           10  MONAT           PIC XX.
           10  JAHR            PIC X(4).
           10  FILLER          PIC X.
           10  DIREKTION       PIC 9(4).
           10  FILLER          PIC X.
      *    ANZ-GUTACHT = Anzahl gerechnete Gutachten pro Direktion
           10  ANZ-GUTACHT     PIC 9(10).
