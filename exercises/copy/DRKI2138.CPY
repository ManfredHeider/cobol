      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2138.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2138.CPv  $
      *
      *   Rev 2.2   Feb 27 2009 08:27:46   t13ex51
      *AKI00795: Differenzdeckung in FK Bestandsverträgen
      *
      *   Rev 2.0   28 Sep 2007 14:25:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2138                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2138                    *
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
      *    FÜR DIE FUNKTION DRKI2138
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2138.
           05  PI-DRKI2138-KEY.


             10 PI-DRKI2138-FKVVERTRAGSID   PIC X(13).
             10 PI-DRKI2138-FKVVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2138.
           05  PO-DRKI2138-RC                 PIC  X(003).
               88 PO-DRKI2138-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2138-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2138-DATEN.


             10 PO-DRKI2138-FKVBEDINGUNG   PIC X(20).
             10 PO-DRKI2138-FKVEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2138-FKVGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2138-FKVGESELLSCH-KTY   PIC S9(4) COMP.
             10 PO-DRKI2138-FKVKUENDIGUNGKZ   PIC X(1).
             10 PO-DRKI2138-FKVMAILINGAKTION   PIC X(70).
             10 PO-DRKI2138-FKVTARIFPRODUKT   PIC X(50).
             10 PO-DRKI2138-FKVUMSTELLUNGDAT   PIC X(10).
             10 PO-DRKI2138-FKVVERSNUMMER   PIC X(40).
             10 PO-DRKI2138-FKVZUSATZINFO   PIC X(100).
             10 PO-DRKI2138-LETZTEAENDERUNG   PIC X(26).
             

      *
      ******* ENDE COPY-MEMBER **************************************
