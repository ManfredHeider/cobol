      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1447.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1447.CPv  $
      *
      *   Rev 2.1   Sep 30 2002 14:08:50   t13ex49
      *FKI00661 - erstmalige Übergabe
      *
      *   Rev 2.0   Sep 12 2002 08:29:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1447                                      *
      *    --------------------------------------------------------   *
      *    VB-Schnittstelle - Rückstufungsdatum vervollständigen      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                       *
      *    DATUM:    23. 04. 2002                                     *
      *****************************************************************
      *
      *    VERSION  1 - Länge 137 Byte
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00661!25.09.02!    ! NEU                       !Peter Pohl*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *--------------------------------------------------------------*
       01  DRKI1447.
           05 Filler                       PIC X(03).
           05 DRKI1447-VB-Nummer           PIC X(07).
      *       MA_ST_NR
           05 DRKI1447-Stufe               PIC X(05).
      *       MS_NR
           05 DRKI1447-BS-NR               PIC X(05).
      *       BS_NR
           05 DRKI1447-DAT-VON             PIC X(26).
      *       MSBZ_DAT_VON
      *    05 DRKI1447-LAST-UPDATE         PIC X(26).
           05 DRKI1447-LAST-UPDATE.
              10 DRKI1447-Update-Datum     PIC X(10).
              10 FILLER                    PIC X(16).
      *       MSBZ_LAST_UPDATE
           05 DRKI1447-DAT-BIS             PIC X(10).
      *       MSBZ_DAT_BIS
      *---------------------------------------------------------------*

