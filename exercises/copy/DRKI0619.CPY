      * P13SP99   Stand: 06/12/98    18:02:15
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0619.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0619.CPv  $
      *
      *   Rev 2.1   Apr 22 1999 17:06:48   T13EX48
      * 
      *
      *   Rev 2.0   Apr 22 1999 16:49:14   T13EX48
      * 
      *
      *   Rev 1.0   25 Aug 1998 09:24:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0619  DATEIAUFBAU RUECKDTA FEHLERPROT.   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    18.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!13.08.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DATENSATZBESCHREIBUNG FUER            *
      *    DAS FEHLERPROTOKOLL DIREKTIONSAENDERUNG (RUECKDTA).       *
      *    DATENSATZLAENGE 67                                        *
      *                                                              *
      ****************************************************************
      *
       01  DRKI0619-SATZ.
           10 DRKI0619-DIRAENDID              pic X(013).
           10 DRKI0619-DIRAENDPERSONENID      pic X(013).
           10 DRKI0619-DIRAENDPERSROLLE       pic X(001).
           10 DRKI0619-DIRAENDDTAERLEDIGT     pic X(001).
           10 DRKI0619-AENDERUNGSKZ           pic 9(002).
           10 DRKI0619-VERTRAGSNR             pic 9(015).
           10 DRKI0619-DIREKTIONSNR           pic 9(003).
           10 DRKI0619-KUNDENBETREUERNR       pic 9(007).
           10 DRKI0619-INTVERTRAGSNR          PIC 9(007).
           10 DRKI0619-PRUEFZIFFER            PIC 9(002).
           10 DRKI0619-PARTGESELLSCHAFTID     PIC 9(003).



