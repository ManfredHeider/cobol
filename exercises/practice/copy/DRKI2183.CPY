      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2183.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2183.CPv  $
      *
      *   Rev 2.2   29 Oct 2007 16:49:56   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.1   17 Oct 2007 13:16:10   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 12:12:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2183                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2183                    *
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
      *    FÜR DIE FUNKTION DRKI2183
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2183.
           05  PI-DRKI2183-KEY.


             10 PI-DRKI2183-VLTLEISTUNGID   PIC S9(4) COMP.
             10 PI-DRKI2183-VLTRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2183-VLTTHEMGSCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2183-VLTVERTRAGSID   PIC X(13).
             10 PI-DRKI2183-VLTVETLFDNR   PIC S9(4) COMP.

      *
       01  PO-DRKI2183.
           05  PO-DRKI2183-RC                 PIC  X(003).
               88 PO-DRKI2183-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2183-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2183-DATEN.


             10 PO-DRKI2183-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2183-VLTBEZEICHNUNG   PIC X(50).
             10 PO-DRKI2183-VLTEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2183-VLTLEISTUNGSTXT   PIC X(30).
             10 PO-DRKI2183-VLTSONSTWERT   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2183-VLTSONSTWERTEINH   PIC X(20).
             10 PO-DRKI2183-VLTTHEMGSCHL-KTY   PIC S9(4) COMP.
             10 PO-DRKI2183-VLTWERT   PIC S9(9)V9(2) COMP-3.


      *
      ******* ENDE COPY-MEMBER **************************************
