      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2143.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2143.CPv  $
      *
      *   Rev 2.2   Jan 28 2009 16:07:52   t13ex51
      *AKI00772: Reisegepäckversicherung Aktualisierung
      *
      *   Rev 2.0   28 Sep 2007 14:40:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2143                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2143                    *
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
      *    FÜR DIE FUNKTION DRKI2143
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2143.
           05  PI-DRKI2143-KEY.


             10 PI-DRKI2143-RGPVERTRAGSID   PIC X(13).
             10 PI-DRKI2143-RGPVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2143.
           05  PO-DRKI2143-RC                 PIC  X(003).
               88 PO-DRKI2143-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2143-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2143-DATEN.


             10 PO-DRKI2143-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2143-RGPBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2143-RGPBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PO-DRKI2143-RGPDIFDECKKZ   PIC X(1).
             10 PO-DRKI2143-RGPEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2143-RGPGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2143-RGPGESELLSCH-KTY   PIC S9(4) COMP.
             10 PO-DRKI2143-RGPKUENDIGUNGKZ   PIC X(1).
             10 PO-DRKI2143-RGPMAILING-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2143-RGPMAILING-KTY   PIC S9(4) COMP.
             10 PO-DRKI2143-RGPVERSNUMMER   PIC X(40).
             10 PO-DRKI2143-RGPVERSSUMME   PIC S9(9) COMP.
             10 PO-DRKI2143-RGPVUMSTELLUNGDAT   PIC X(10).
             10 PO-DRKI2143-RGPZUSINFO-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2143-RGPZUSINFO-KTY   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
