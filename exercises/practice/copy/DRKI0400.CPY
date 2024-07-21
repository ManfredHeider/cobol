      * P13SP99   Stand: 06/12/98    18:01:26
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0400.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0400.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 12:06:36   T13EX49
      *AKI00028 - Prod.übernahme
      *
      *   Rev 1.0   14 Jul 1998 17:04:58   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0400  FUNKTIONEN STANDARD-OZP            *
      *                           DIREKTIONSÄNDERUNG                 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    23.07.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      * AKI00006!19.08.98! 02 ! RUECK-DTA-EINFUEHRUNG     ! BRENNECKE*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0081.
           05 PI-ZTKI0081-FUNKTION             PIC X(008).
              88 PI-ZTKI0081-DRKI0401-INS            VALUE 'DRKI0401'.
              88 PI-ZTKI0081-DRKI0591-UPD            VALUE 'DRKI0591'.
