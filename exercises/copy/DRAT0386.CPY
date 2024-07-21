      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0386.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRAT0386.CPv  $
      *
      *   Rev 2.3   Mar 03 2003 16:41:28   t13ae34
      *AAT00023-Schnittstellen Host-Kompetenzen
      *
      *   Rev 2.2   Feb 07 2003 11:45:18   t13ae34
      *AAT00023-Batch-SST STATnnnn
      *
      *   Rev 2.1   Dec 13 2002 10:21:06   t13ae34
      *AAT00023 - Satzbeschreibung STAT0003
      *
      *   Rev 2.0   26 Jun 2001 12:55:08   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRAT0386  K&S STAT0003 - SST HOSTKOMPETENZEN  * 00020000
      *                           AUßENDIENST                         * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: HEIDER                                           * 00060000
      *    DATUM:    12.12.2002                                       * 00070000
      *                                                               * 00080100
      ***************************************************************** 00084000
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00023!12.12.02!2.1 ! NEU                       ! HEIDER   *
      * AAT00023!27.02.03!2.2 ! MIT VB                    ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  STAT0003S.
           02 STAT0003-USERID            PIC  X(07).                       007
           02 STAT0003-SEM-1             PIC  X(01).                       008
           02 STAT0003-VB                PIC  9(07).                       015
           02 STAT0003-SEM-2             PIC  X(01).                       016
           02 STAT0003-NAME              PIC  X(30).                       046
           02 STAT0003-SEM-3             PIC  X(01).                       047
           02 STAT0003-VORNAME           PIC  X(20).                       067
           02 STAT0003-SEM-4             PIC  X(01).                       068
           02 STAT0003-DIR               PIC  X(03).                       071
           02 STAT0003-SEM-5             PIC  X(01).                       072
           02 STAT0003-BENGRUPPROFIL     PIC  X(80).                       152
           02 STAT0003-SEM-6             PIC  X(01).                       153
           02 STAT0003-UU11.
              03 STAT0003-UU11-BILD1     PIC  X(01).                       154
              03 STAT0003-SEM-7          PIC  X(01).                       155
              03 STAT0003-UU11-BILD2     PIC  X(01).                       156
              03 STAT0003-SEM-8          PIC  X(01).                       157
              03 STAT0003-UU11-BILD3     PIC  X(01).                       158
              03 STAT0003-SEM-9          PIC  X(01).                       159
              03 STAT0003-UU11-BILD4     PIC  X(01).                       160
              03 STAT0003-SEM-10         PIC  X(01).                       161
           02 STAT0003-ANZDIA            PIC  9(03).                       164
           02 STAT0003-SEM-11            PIC  X(01).                       165
           02 STAT0003-DIALOG-TAB.
              03 STAT0003-DIALOG-T OCCURS 300.                            1965
                 04 STAT0003-DIALOG      PIC  X(04).
                 04 STAT0003-PROFIL      PIC  X(01).
                 04 STAT0003-STERN       PIC  X(01).
