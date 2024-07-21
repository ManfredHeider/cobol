      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0759.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0759.CPv  $
      *
      *   Rev 2.3   Dec 09 2019 13:56:02   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.2   Jan 11 2012 09:08:42   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.0   Dec 20 2011 08:57:03 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0759                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0759                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.11.2019 / 10:12:03 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI0759
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0759.
           05  PI-DRKI0759-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0759-VRTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0759-VRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0759-VRTVERTART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0759-VRTVERTART-KTY   PIC S9(4) COMP.
             10 PI-DRKI0759-VRTVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI0759.
           05  PO-DRKI0759-RC                     PIC  X(003).
               88 PO-DRKI0759-RC-OK               VALUE 'OK '.
               88 PO-DRKI0759-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

