      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2175.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2175.CPv  $
      *
      *   Rev 2.2   Feb 27 2009 08:28:12   t13ex51
      *AKI00795: Differenzdeckung in FK Bestandsverträgen
      *
      *   Rev 2.0   29 Sep 2007 11:48:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2175                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2175                    *
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
      *    FÜR DIE FUNKTION DRKI2175
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2175.
           05  PI-DRKI2175-KEY.


             10 PI-DRKI2175-VRGRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2175-VRGVERTRAGSID   PIC X(13).
             10 PI-DRKI2175-VRGVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2175-DATEN.


             10 PI-DRKI2175-VRGABLAUFRISGRUP   PIC X(10).
             10 PI-DRKI2175-VRGBEGINNRISGRUP   PIC X(10).
             10 PI-DRKI2175-VRGBEZEICHNUNG   PIC X(50).
             10 PI-DRKI2175-VRGDIFDECKKZ   PIC X(1).
             10 PI-DRKI2175-VRGEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2175-VRGGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2175-VRGKUENDIGUNGKZ   PIC X(1).
             10 PI-DRKI2175-VRGNETTOBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2175-VRGRISIGRTYP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2175-VRGRISIKOHOSTID   PIC S9(9) COMP.
             10 PI-DRKI2175-VRGUBERGEOGRUPID   PIC S9(4) COMP.
             10 PI-DRKI2175-VRGUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2175-VRGVERSNUMMER   PIC X(40).
             
      *
       01  PO-DRKI2175.
           05  PO-DRKI2175-RC                 PIC  X(003).
               88 PO-DRKI2175-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2175-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
