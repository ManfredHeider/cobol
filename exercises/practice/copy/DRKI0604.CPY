      * P13SP99   Stand: 05/10/98    11:42:01
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0604.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0604.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 13:17:38   T13EX19
      *aki00028
      *
      *   Rev 1.0   25 Aug 1998 09:22:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0604 Funktion SELECT                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    23.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!23.07.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0604                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       JOZP VBDATEN ERMITTELN             *
      ****************************************************************
      *
       01 PI-DRKI0604.
          05 PI-DRKI0604-KEY.
             10 PI-DRKI0604-VRBVERTRAGSID        PIC X(13).
      *
       01 PO-DRKI0604.
          05 PO-DRKI0604-RC                      PIC  X(003).
             88 PO-DRKI0604-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0604-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI0604-DATEN.
             10 PO-DRKI0604-ADMADIREKTION        PIC  9(003).
             10 PO-DRKI0604-MITARBEITERNR        PIC  9(007).
      *
      ****** Ende Copy-Member ****************************************
