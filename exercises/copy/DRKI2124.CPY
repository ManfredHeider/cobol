      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2124.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2124.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:02:20   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 13:16:42   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2124                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2124                    *
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
      *    FÜR DIE FUNKTION DRKI2124
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2124.
           05  PI-DRKI2124-KEY.


             10 PI-DRKI2124-UNFVERTRAGSID   PIC X(13).
             10 PI-DRKI2124-UNFVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2124-DATEN.


             10 PI-DRKI2124-UNFANZVERSPERSON   PIC S9(9) COMP.
             10 PI-DRKI2124-UNFBEDINGUNG   PIC X(16).
             10 PI-DRKI2124-UNFEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2124-UNFGESELLSCHAFT   PIC X(50).
             10 PI-DRKI2124-UNFKLAUSELTARIF   PIC X(80).
             10 PI-DRKI2124-UNFKUENDIGUNGKZ   PIC X(2).
             10 PI-DRKI2124-UNFLEISTFREIBDAT   PIC X(10).
             10 PI-DRKI2124-UNFLEISTFREIVDAT   PIC X(10).
             10 PI-DRKI2124-UNFMAILINGAKTION   PIC X(70).
             10 PI-DRKI2124-UNFMINDBEITRAGKZ   PIC X(2).
             10 PI-DRKI2124-UNFRISIKO   PIC X(50).
             10 PI-DRKI2124-UNFSELBSTBFEST   PIC S9(9) COMP.
             10 PI-DRKI2124-UNFSELBSTBKZ   PIC X(2).
             10 PI-DRKI2124-UNFSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2124-UNFSELBSTBTEXT   PIC X(50).
             10 PI-DRKI2124-UNFUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2124-UNFVERSNUMMER   PIC X(40).
             10 PI-DRKI2124-UNFZUSATZINFO   PIC X(100).
             
      *
       01  PO-DRKI2124.
           05  PO-DRKI2124-RC                 PIC  X(003).
               88 PO-DRKI2124-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2124-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
