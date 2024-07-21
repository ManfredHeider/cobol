      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0318.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0318.cpv  $
      *
      *   Rev 2.1   Mar 09 2000 17:30:44   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.0   Mar 16 1999 09:57:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     ****************************************************************
      *    COPY-MEMBER: DRAT0318  KONSTANTEN FÜR GEVO   KTY = 117    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M. WILKENS                                      *
      *    DATUM:    01.03.2000                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00087!01.03.00! 01 ! NEUERSTELLUNG             ! WILKENS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-GEVO-NEUZUGANG         PIC S9(004) BINARY SYNC
                                                   VALUE  0001.
           05 K-GEVO-FOLGE             PIC S9(004) BINARY SYNC
                                                   VALUE  0002.
           05 K-GEVO-NEUTRAL           PIC S9(004) BINARY SYNC
                                                   VALUE  0003.
           05 K-GEVO-WIEDERINKRAFT     PIC S9(004) BINARY SYNC
                                                   VALUE  0004.
           05 K-GEVO-RUECKBELASTUNG    PIC S9(004) BINARY SYNC
                                                   VALUE  0005.
           05 K-GEVO-EVS               PIC S9(004) BINARY SYNC
                                                   VALUE  0006.
           05 K-GEVO-EVS-STORNO-WIK    PIC S9(004) BINARY SYNC
                                                   VALUE  0007.
           05 K-GEVO-UNDEFINIERT       PIC S9(004) BINARY SYNC
                                                   VALUE  9999.
