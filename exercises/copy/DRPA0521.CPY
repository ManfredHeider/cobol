      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0521.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0521.CPv  $
      *
      *   Rev 2.2   05 Feb 2001 10:21:36   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.1   May 27 1999 09:22:24   T13EX25
      *APA00042, Korrektur V2.0
      *
      *   Rev 2.0   May 27 1999 08:41:42   T13EX25
      *APA00042, Erfassung V2.0
      *
      *
      *   Rev 1.0   12 Sep 1997 16:59:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0521  FUNKTION                           *
      *                        "Umrechnungskurs ermitteln"           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    24.09.1997                                      *
      ****************************************************************
      *    VERSION   02                                               
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !24.09.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      * APA00118!23.01.01! 2.1! NEUE FUNKTION DRPA1466    ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0067.                                               
           05 PI-UPPA0067-FUNKTION            PIC X(008).             
              88 PI-UPPA0067-DRPA0522             VALUE 'DRPA0522'.
230101        88 PI-UPPA0067-DRPA1466             VALUE 'DRPA1466'.
