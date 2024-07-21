      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2180.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2180.CPv  $
      *
      *   Rev 2.2   29 Oct 2007 16:49:18   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.1   17 Oct 2007 13:15:34   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 12:03:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2180                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2180                    *
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
      *    FÜR DIE FUNKTION DRKI2180
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2180.
           05  PI-DRKI2180-KEY.


             10 PI-DRKI2180-VLTLEISTUNGID   PIC S9(4) COMP.
             10 PI-DRKI2180-VLTRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2180-VLTTHEMGSCHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2180-VLTVERTRAGSID   PIC X(13).
             10 PI-DRKI2180-VLTVETLFDNR   PIC S9(4) COMP.


           05  PI-DRKI2180-DATEN.


             10 PI-DRKI2180-VLTBEZEICHNUNG   PIC X(50).
             10 PI-DRKI2180-VLTEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2180-VLTLEISTUNGSTXT   PIC X(30).
             10 PI-DRKI2180-VLTSONSTWERT   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2180-VLTSONSTWERTEINH   PIC X(20).
             10 PI-DRKI2180-VLTWERT   PIC S9(9)V9(2) COMP-3.

      *
       01  PO-DRKI2180.
           05  PO-DRKI2180-RC                 PIC  X(003).
               88 PO-DRKI2180-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2180-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
