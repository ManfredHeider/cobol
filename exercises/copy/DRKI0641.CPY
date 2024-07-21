      * P13SP99   Stand: 06/12/98    18:03:16
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0641.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0641.CPv  $
      *
      *   Rev 2.1   Apr 22 1999 17:02:54   T13EX48
      *AKI00028
      *RDTA
      *
      *   Rev 2.0   Apr 22 1999 16:49:34   T13EX48
      * 
      *
      *   Rev 1.0   25 Aug 1998 09:26:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0641  DATEIAUFBAU RUECKDTA AMVERS        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    18.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.08.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      *         !        !    !                           !          *
      * Fehler  !08.03.99!    ! VNR wird falsch belegt da !HSE       *
      *         !        !    ! MOVE v. PIC 9(15) -> X(14)!          *
      *         !        !    ! ==> Letzte Ziffer fehlt   !          *
      *         !        !    ! SA-AMS umdefiniert        !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * SATZLAENGE: 48                                               *
      *                                                              *
      *                                                              *
      ****************************************************************
      *
       01  DRKI0641-SATZ.
990308*    10 DRKI0641-SA-AMS                 pic X(004).
990308     10 DRKI0641-SA-AMS                 PIC 9(004).
           10 FILLER                          PIC X(011) VALUE SPACES.
990308*    10 DRKI0641-VNR-AMS                PIC X(014).
990308     10 DRKI0641-VNR-AMS                PIC 9(014).
           10 DRKI0641-PARTINFO-AMS.
              20 DRKI0641-PFILLER             PIC X(007) VALUE SPACES.
              20 DRKI0641-PDIR-AMS            PIC X(003).
              20 DRKI0641-PVBNR-AMS           PIC X(006).
           10 FILLER                          PIC X(003) VALUE SPACES.
