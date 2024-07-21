      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0201.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI0201.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 11:11:10   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 1.3   09 Feb 1999 12:31:48   T13EX12
      *AKI00014 - Update-Funktion eingebaut
      *
      *   Rev 1.2   29 Oct 1998 17:47:18   T13EX19
      *FKI00010 - Korrektion
      *
      *   Rev 1.1   12 Aug 1998 16:47:02   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:47:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0201  FUNKTIONEN STANDARD-OZP            *
      *                           PERSONADRESSBEZ                    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    22.06.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!22.06.98! 01 ! NEUERSTELLUNG             ! Veit     *
      * -------------------------------------------------------------*
      * AKI00001!14.10.98! 02 ! SEL-AAI und DEL eingebaut ! gbe      *
      * -------------------------------------------------------------*
      * AKI00001!01.11.98! 03 ! Update Funktion eingebaut !v.d.Goltz *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0044.
           05 PI-ZTKI0044-FUNKTION             PIC X(008).
              88 PI-ZTKI0044-DRKI0202-SEL-ALL      VALUE 'DRKI0202'.
              88 PI-ZTKI0044-DRKI0725-SEL-AAI      VALUE 'DRKI0725'.
              88 PI-ZTKI0044-DRKI0203-INS          VALUE 'DRKI0203'.
              88 PI-ZTKI0044-DRKI0727-DEL          VALUE 'DRKI0727'.
011198        88 PI-ZTKI0044-DRKI0726-UPD          VALUE 'DRKI0726'.
              88   PI-ZTKI0044-DRKI1574     VALUE 'DRKI1574'.
              88   PI-ZTKI0044-DRKI1573     VALUE 'DRKI1573'.
              88   PI-ZTKI0044-DRKI1575     VALUE 'DRKI1575'.

