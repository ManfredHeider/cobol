      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2178.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2178.CPv  $
      *
      *   Rev 2.2   Feb 27 2009 08:28:26   t13ex51
      *AKI00795: Differenzdeckung in FK Bestandsverträgen
      *
      *   Rev 2.0   29 Sep 2007 11:57:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2178                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2178                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.02.2009/17:57:33
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
      *    FÜR DIE FUNKTION DRKI2178
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2178.
           05  PI-DRKI2178-KEY.


             10 PI-DRKI2178-VRGRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2178-VRGVERTRAGSID   PIC X(13).
             10 PI-DRKI2178-VRGVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2178.
           05  PO-DRKI2178-RC                 PIC  X(003).
               88 PO-DRKI2178-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2178-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2178-DATEN.


             10 PO-DRKI2178-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2178-VRGABLAUFRISGRUP   PIC X(10).
             10 PO-DRKI2178-VRGBEGINNRISGRUP   PIC X(10).
             10 PO-DRKI2178-VRGBEZEICHNUNG   PIC X(50).
             10 PO-DRKI2178-VRGDIFDECKKZ   PIC X(1).
             10 PO-DRKI2178-VRGEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2178-VRGGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2178-VRGGESELLSCH-KTY   PIC S9(4) COMP.
             10 PO-DRKI2178-VRGKUENDIGUNGKZ   PIC X(1).
             10 PO-DRKI2178-VRGNETTOBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2178-VRGRISIGRTYP-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2178-VRGRISIGRTYP-KTY   PIC S9(4) COMP.
             10 PO-DRKI2178-VRGRISIKOHOSTID   PIC S9(9) COMP.
             10 PO-DRKI2178-VRGUBERGEOGRUPID   PIC S9(4) COMP.
             10 PO-DRKI2178-VRGUMSTELLUNGDAT   PIC X(10).
             10 PO-DRKI2178-VRGVERSNUMMER   PIC X(40).
             

      *
      ******* ENDE COPY-MEMBER **************************************
