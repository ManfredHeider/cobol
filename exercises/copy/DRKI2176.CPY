      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2176.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2176.CPv  $
      *
      *   Rev 2.2   Feb 27 2009 08:28:18   t13ex51
      *AKI00795: Differenzdeckung in FK Bestandsverträgen
      *
      *   Rev 2.0   29 Sep 2007 11:51:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2176                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2176                    *
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
      *    FÜR DIE FUNKTION DRKI2176
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2176.
           05  PI-DRKI2176-KEY.


             10 PI-DRKI2176-VRGRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2176-VRGVERTRAGSID   PIC X(13).
             10 PI-DRKI2176-VRGVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2176-DATEN.


             10 PI-DRKI2176-VRGABLAUFRISGRUP   PIC X(10).
             10 PI-DRKI2176-VRGBEGINNRISGRUP   PIC X(10).
             10 PI-DRKI2176-VRGBEZEICHNUNG   PIC X(50).
             10 PI-DRKI2176-VRGDIFDECKKZ   PIC X(1).
             10 PI-DRKI2176-VRGEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2176-VRGGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2176-VRGKUENDIGUNGKZ   PIC X(1).
             10 PI-DRKI2176-VRGNETTOBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2176-VRGRISIGRTYP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2176-VRGRISIKOHOSTID   PIC S9(9) COMP.
             10 PI-DRKI2176-VRGUBERGEOGRUPID   PIC S9(4) COMP.
             10 PI-DRKI2176-VRGUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2176-VRGVERSNUMMER   PIC X(40).
             

      *
       01  PO-DRKI2176.
           05  PO-DRKI2176-RC                 PIC  X(003).
               88 PO-DRKI2176-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2176-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
