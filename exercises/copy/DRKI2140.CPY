      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2140.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2140.CPv  $
      *
      *   Rev 2.2   Jan 28 2009 16:07:30   t13ex51
      *AKI00772: Reisegepäckversicherung Aktualisierung
      *
      *   Rev 2.0   28 Sep 2007 14:31:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2140                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2140                    *
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
      *    FÜR DIE FUNKTION DRKI2140
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2140.
           05  PI-DRKI2140-KEY.


             10 PI-DRKI2140-RGPVERTRAGSID   PIC X(13).
             10 PI-DRKI2140-RGPVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2140-DATEN.


             10 PI-DRKI2140-RGPBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2140-RGPBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PI-DRKI2140-RGPDIFDECKKZ   PIC X(1).
             10 PI-DRKI2140-RGPEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2140-RGPGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2140-RGPGESELLSCH-KTY   PIC S9(4) COMP.
             10 PI-DRKI2140-RGPKUENDIGUNGKZ   PIC X(1).
             10 PI-DRKI2140-RGPMAILING-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2140-RGPMAILING-KTY   PIC S9(4) COMP.
             10 PI-DRKI2140-RGPVERSNUMMER   PIC X(40).
             10 PI-DRKI2140-RGPVERSSUMME   PIC S9(9) COMP.
             10 PI-DRKI2140-RGPVUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2140-RGPZUSINFO-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2140-RGPZUSINFO-KTY   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2140.
           05  PO-DRKI2140-RC                 PIC  X(003).
               88 PO-DRKI2140-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2140-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
