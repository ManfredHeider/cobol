      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2141.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2141.CPv  $
      *
      *   Rev 2.2   Jan 28 2009 16:07:38   t13ex51
      *AKI00772: Reisegepäckversicherung Aktualisierung
      *
      *   Rev 2.0   28 Sep 2007 14:34:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2141                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2141                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  22.12.2008/15:45:42
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2141
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2141.
           05  PI-DRKI2141-KEY.


             10 PI-DRKI2141-RGPVERTRAGSID   PIC X(13).
             10 PI-DRKI2141-RGPVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2141-DATEN.


             10 PI-DRKI2141-RGPBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2141-RGPDIFDECKKZ   PIC X(1).
             10 PI-DRKI2141-RGPEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2141-RGPGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2141-RGPKUENDIGUNGKZ   PIC X(1).
             10 PI-DRKI2141-RGPMAILING-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2141-RGPVERSNUMMER   PIC X(40).
             10 PI-DRKI2141-RGPVERSSUMME   PIC S9(9) COMP.
             10 PI-DRKI2141-RGPVUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2141-RGPZUSINFO-KNR   PIC S9(15)V COMP-3.
             

      *
       01  PO-DRKI2141.
           05  PO-DRKI2141-RC                 PIC  X(003).
               88 PO-DRKI2141-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2141-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
