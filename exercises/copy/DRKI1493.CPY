      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1493.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1493.CPv  $
      *
      *   Rev 2.2   Nov 24 2020 09:02:34   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.1   Jan 27 2003 11:01:12   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.0   Oct 16 2002 08:33:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1493                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1493                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:35 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      *---------!--------!--------------------------------!----------*
      * AKI01805!13.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1493
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1493.
           05  PI-DRKI1493-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1493-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1493-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI1493-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI1493-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI1493-GHPESAFE   PIC  X(0001).
             10 PI-DRKI1493-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI1493-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI1493-GHPTELEFONREVERS   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI1493-NI1.
           05 PI-DRKI1493-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI1493-NI7.
           05 PI-DRKI1493-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI1493.
           05  PO-DRKI1493-RC                     PIC  X(003).
               88 PO-DRKI1493-RC-OK               VALUE 'OK '.
               88 PO-DRKI1493-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
