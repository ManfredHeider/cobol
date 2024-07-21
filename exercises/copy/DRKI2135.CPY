      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2135.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2135.CPv  $
      *
      *   Rev 2.2   Feb 27 2009 08:27:40   t13ex51
      *AKI00795: Differenzdeckung in FK Bestandsverträgen
      *
      *   Rev 2.0   28 Sep 2007 13:50:00   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2135                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2135                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.02.2009/17:53:36
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
      *    FÜR DIE FUNKTION DRKI2135
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2135.
           05  PI-DRKI2135-KEY.


             10 PI-DRKI2135-FKVVERTRAGSID   PIC X(13).
             10 PI-DRKI2135-FKVVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2135-DATEN.


             10 PI-DRKI2135-FKVBEDINGUNG   PIC X(20).
             10 PI-DRKI2135-FKVEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2135-FKVGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2135-FKVKUENDIGUNGKZ   PIC X(1).
             10 PI-DRKI2135-FKVMAILINGAKTION   PIC X(70).
             10 PI-DRKI2135-FKVTARIFPRODUKT   PIC X(50).
             10 PI-DRKI2135-FKVUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2135-FKVVERSNUMMER   PIC X(40).
             10 PI-DRKI2135-FKVZUSATZINFO   PIC X(100).
             

      *
       01  PO-DRKI2135.
           05  PO-DRKI2135-RC                 PIC  X(003).
               88 PO-DRKI2135-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2135-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
