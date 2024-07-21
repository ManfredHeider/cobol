      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1202.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1202.CPv  $
      *
      *   Rev 2.1   05 Apr 2001 10:22:00   T13AE44
      * 
      *
      *   Rev 2.0   12 Mar 2001 08:55:14   p13sp16
      *Archiverstellung
      *
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    LAENGE =  28 Bytes       ERSTELLT AM 13.03.01.              *
      *                                                                *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      * AKI174 ! NEU                                   !Biehl    ! 01  *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1202.
         05 DRKI1202-ZEILE1.
           10  MIS-MONAT-VON   PIC XX.
           10  MIS-STRICH      PIC X.
           10  MIS-MONAT-BIS   PIC XX.
           10  FILLER          PIC X.
           10  MIS-JAHR        PIC X(4).
           10  FILLER          PIC X.
           10  MIS-IST         PIC X.
           10  MIS-DIREKTION   PIC X(4).
           10  FILLER          PIC X(12).
         05 DRKI1202-ZEILE2    REDEFINES DRKI1202-ZEILE1.
           10  MIS-BEZ         PIC X(8).
           10  FILLER          PIC X(10).
           10  MIS-ANZ-GUTACHT PIC X(10).
         05 DRKI1202-ZEILE3    REDEFINES DRKI1202-ZEILE1.
           10  MIS-LEERZEILE   PIC X(28).
      *
      *---------------------------------------------------------------*
