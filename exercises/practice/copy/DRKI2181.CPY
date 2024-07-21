      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2181.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2181.CPv  $
      *
      *   Rev 2.2   29 Oct 2007 16:49:32   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.1   17 Oct 2007 13:15:46   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 12:06:42   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2181                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2181                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.10.2007/12:14:04
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
      *    FÜR DIE FUNKTION DRKI2181
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2181.
           05  PI-DRKI2181-KEY.


             10 PI-DRKI2181-VLTLEISTUNGID   PIC S9(4) COMP.
             10 PI-DRKI2181-VLTRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2181-VLTTHEMGSCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2181-VLTVERTRAGSID   PIC X(13).
             10 PI-DRKI2181-VLTVETLFDNR   PIC S9(4) COMP.


           05  PI-DRKI2181-DATEN.


             10 PI-DRKI2181-VLTBEZEICHNUNG   PIC X(50).
             10 PI-DRKI2181-VLTEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2181-VLTLEISTUNGSTXT   PIC X(30).
             10 PI-DRKI2181-VLTSONSTWERT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2181-VLTSONSTWERTEINH   PIC X(20).
             10 PI-DRKI2181-VLTWERT   PIC S9(9)V9(2) COMP-3.


      *
       01  PO-DRKI2181.
           05  PO-DRKI2181-RC                 PIC  X(003).
               88 PO-DRKI2181-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2181-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
