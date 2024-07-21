      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0388.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRAT0388.CPv  $
      *
      *   Rev 2.1   Feb 07 2003 11:45:20   t13ae34
      *AAT00023-Batch-SST STATnnnn
      *
      *   Rev 2.0   26 Jun 2001 12:55:10   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0388  K&S STAT0004 - SST HOSTKOMPETENZEN  *
      *                           AUSNAHMEN                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    28.01.2003                                       *
      *                                                               *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00023!18.01.03!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  STAT0004S.
           02 STAT0004-USERID            PIC  X(07).                       007
           02 STAT0004-SEM-1             PIC  X(01).                       008
           02 STAT0004-NAME              PIC  X(30).                       038
           02 STAT0004-SEM-2             PIC  X(01).                       039
           02 STAT0004-VORNAME           PIC  X(20).                       059
           02 STAT0004-SEM-3             PIC  X(01).                       060
           02 STAT0004-BENGRUPPROFIL     PIC  X(80).                       140
           02 STAT0004-SEM-4             PIC  X(01).                       141
           02 STAT0004-UU11.
              03 STAT0004-UU11-BILD1     PIC  X(01).                       142
              03 STAT0004-SEM-5          PIC  X(01).                       141
              03 STAT0004-UU11-BILD2     PIC  X(01).                       144
              03 STAT0004-SEM-6          PIC  X(01).                       145
              03 STAT0004-UU11-BILD3     PIC  X(01).                       146
              03 STAT0004-SEM-7          PIC  X(01).                       147
              03 STAT0004-UU11-BILD4     PIC  X(01).                       148
              03 STAT0004-SEM-8          PIC  X(01).                       149
           02 STAT0004-ANZDIA            PIC  9(03).                       152
           02 STAT0004-SEM-9             PIC  X(01).                       153
           02 STAT0004-DIALOG-TAB.
              03 STAT0004-DIALOG-T OCCURS 300.                            1953
                 04 STAT0004-DIALOG      PIC  X(04).
                 04 STAT0004-PROFIL      PIC  X(01).
                 04 STAT0004-STERN       PIC  X(01).
