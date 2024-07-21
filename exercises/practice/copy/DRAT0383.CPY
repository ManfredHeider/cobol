      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0383.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRAT0383.CPv  $
      *
      *   Rev 2.2   Apr 07 2003 11:39:58   t13ae34
      *AAT00023-hier: STAT0002 mit Name
      *
      *   Rev 2.1   Dec 06 2002 09:49:24   t13ae34
      *AAT00023 - hier: Schnittstelle STAT0002
      *
      *   Rev 2.0   26 Jun 2001 12:55:06   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRAT0383  K&S STAT0002 - SST HOSTKOMPETENZEN  * 00020000
      *                           ZUM DATENPOOL                       * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: HEIDER                                           * 00060000
      *    DATUM:    15.10.2002                                       * 00070000
      *                                                               * 00080100
      ***************************************************************** 00084000
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00023!15.10.02!2.1 ! NEU                       ! HEIDER   *
      * AAT00023!27.03.03!2.2 ! MIT FELD NAME             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  STAT0002S.
           02 STAT0002-PERSNR            PIC  9(10).
           02 STAT0002-USERID            PIC  X(07).
270303     02 STAT0002-NAME              PIC  X(22).
           02 STAT0002-BENGRUPPROFIL     PIC  X(80).
           02 STAT0002-UU11.
              03 STAT0002-UU11-BILD1     PIC  X(01).
              03 STAT0002-UU11-BILD2     PIC  X(01).
              03 STAT0002-UU11-BILD3     PIC  X(01).
              03 STAT0002-UU11-BILD4     PIC  X(01).
           02 STAT0002-ANZDIA            PIC  9(03).
           02 STAT0002-DIALOG-TAB.
              03 STAT0002-DIALOG-T OCCURS 300.
                 04 STAT0002-DIALOG      PIC  X(04).
                 04 STAT0002-PROFIL      PIC  X(01).
