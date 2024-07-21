      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2127.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2127.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:02:42   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 13:25:36   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2127                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2127                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.02.2008/18:51:32
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
      *    FÜR DIE FUNKTION DRKI2127
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2127.
           05  PI-DRKI2127-KEY.


             10 PI-DRKI2127-UNFVERTRAGSID   PIC X(13).
             10 PI-DRKI2127-UNFVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2127.
           05  PO-DRKI2127-RC                 PIC  X(003).
               88 PO-DRKI2127-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2127-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2127-DATEN.


             10 PO-DRKI2127-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2127-UNFANZVERSPERSON   PIC S9(9) COMP.
             10 PO-DRKI2127-UNFBEDINGUNG   PIC X(16).
             10 PO-DRKI2127-UNFEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2127-UNFGESELLSCHAFT   PIC X(50).
             10 PO-DRKI2127-UNFKLAUSELTARIF   PIC X(80).
             10 PO-DRKI2127-UNFKUENDIGUNGKZ   PIC X(2).
             10 PO-DRKI2127-UNFLEISTFREIBDAT   PIC X(10).
             10 PO-DRKI2127-UNFLEISTFREIVDAT   PIC X(10).
             10 PO-DRKI2127-UNFMAILINGAKTION   PIC X(70).
             10 PO-DRKI2127-UNFMINDBEITRAGKZ   PIC X(2).
             10 PO-DRKI2127-UNFRISIKO   PIC X(50).
             10 PO-DRKI2127-UNFSELBSTBFEST   PIC S9(9) COMP.
             10 PO-DRKI2127-UNFSELBSTBKZ   PIC X(2).
             10 PO-DRKI2127-UNFSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2127-UNFSELBSTBTEXT   PIC X(50).
             10 PO-DRKI2127-UNFUMSTELLUNGDAT   PIC X(10).
             10 PO-DRKI2127-UNFVERSNUMMER   PIC X(40).
             10 PO-DRKI2127-UNFZUSATZINFO   PIC X(100).
             

      *
      ******* ENDE COPY-MEMBER **************************************
