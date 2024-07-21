      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2158.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2158.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:04:28   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 17:18:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2158                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2158                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.02.2008/18:54:48
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
      *    FÜR DIE FUNKTION DRKI2158
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2158.
           05  PI-DRKI2158-KEY.


             10 PI-DRKI2158-VGSPVGSBLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2158-VGSVERTRAGSID   PIC X(13).
             10 PI-DRKI2158-VGSVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2158.
           05  PO-DRKI2158-RC                 PIC  X(003).
               88 PO-DRKI2158-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2158-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2158-DATEN.


             10 PO-DRKI2158-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2158-VGSEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2158-VGSSELBSTBART   PIC S9(9) COMP.
             10 PO-DRKI2158-VGSSELBSTBBEZ   PIC X(30).
             10 PO-DRKI2158-VGSSELBSTBFEST   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI2158-VGSSELBSTBMAX   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI2158-VGSSELBSTBMIN   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI2158-VGSSELBSTBPRZ   PIC X(14).
             10 PO-DRKI2158-VGSSELBSTBTEXT   PIC X(30).
             

      *
      ******* ENDE COPY-MEMBER **************************************
