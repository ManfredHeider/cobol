      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1031.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1031.CPv  $
      *
      *   Rev 2.1   25 Apr 2000 10:14:42   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   May 27 1999 14:10:48   T01JM2S
      *APA00042
      *
      *   Rev 1.0   14 Jul 1998 13:59:58   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1031                                      *
      *    --------------------------------------------------------   *
      *    PE-STRUKTURDATEN-TABELLE AUS PAMS                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    31.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!31.07.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00084!14.03.00!2.1 ! KOMPLETTE ÜBERARBEITUNG   ! KRUFF    *
      *         !        !    ! - LÄNGENFELD EINGEFÜGT    !          *
      *         !        !    ! - NEUE STUFENNUMMERN      !          *
      *         !        !    !   VERGEBEN                !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE PE-STRUKTURDATEN-   *
      *    TABELLE AUS PAMS.                                         *
      *                                                              *
      ****************************************************************
      *
           30 PAMS-STRUKTUR-TABELLE.
              32 PAMS-STRUKTUR-TABELLE-LAENGE       PIC S9(004) BINARY.
              32 FILLER                          OCCURS 20
                                                 INDEXED BY IX-PAMS-ST.
                 COPY DRPA1005.
      *
      ******* ENDE COPY-MEMBER **************************************
