      * P13SP99   Stand: 27/09/98    16:34:39
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0600.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0600.CPv  $
      *
      *   Rev 2.1   Apr 22 1999 17:04:58   T13EX48
      *AKI00028
      *RDTA
      *
      *   Rev 2.0   Apr 22 1999 16:49:10   T13EX48
      * 
      *
      *   Rev 1.0   25 Aug 1998 09:22:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0600  DATEIAUFBAU RUECKDTA FEHLERPROT.   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    22.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!22.09.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DATENSATZBESCHREIBUNG FUER            *
      *    DAS FEHLERPROTOKOLL KUNDENBETREUERWECHSEL (RUECKDTA).     *
      *    DATENSATZLAENGE 108                                       *
      *                                                              *
      ****************************************************************
      *
       01  DRKI0600-SATZ.
           10 DRKI0600-VERTRAGSID             pic X(013).
           10 DRKI0600-VBALTID                pic X(013).
           10 DRKI0600-VBALTROLLENKZ          pic X(001).
           10 DRKI0600-VBNEUID                pic X(013).
           10 DRKI0600-VBNEUROLLENKZ          pic X(001).
           10 DRKI0600-DTAERLEDIGTKZ          pic X(001).
           10 DRKI0600-LETZTEAENDERUNG        pic X(026).
           10 DRKI0600-AENDERUNGSKZ           pic 9(002).
           10 DRKI0600-VERTRAGSNR             pic 9(015).
           10 DRKI0600-DIREKTIONSNR           pic 9(003).
           10 DRKI0600-KUNDENBETREUERNR       pic 9(007).
           10 DRKI0600-PARTGESELLSCHAFTID     PIC 9(004).
           10 DRKI0600-INTVERTRAGSNR          PIC 9(007).
           10 DRKI0600-PRUEFZIFFER            PIC 9(002).



