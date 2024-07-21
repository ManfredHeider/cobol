      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2114.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2114.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:01:10   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 14:24:52   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2114                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2114                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.02.2008/18:44:41
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
      *    FÜR DIE FUNKTION DRKI2114
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2114.
           05  PI-DRKI2114-KEY.


             10 PI-DRKI2114-HPVVERTRAGSID   PIC X(13).
             10 PI-DRKI2114-HPVVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2114-DATEN.


             10 PI-DRKI2114-HPVABWEICHENDEVP   PIC X(35).
             10 PI-DRKI2114-HPVAUSLSCHADKZ   PIC X(2).
             10 PI-DRKI2114-HPVBEDINGUNG   PIC X(16).
             10 PI-DRKI2114-HPVDIFFDECKKZ   PIC X(2).
             10 PI-DRKI2114-HPVEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2114-HPVGESELLSCHAFT   PIC X(50).
             10 PI-DRKI2114-HPVKLAUSELTARIF   PIC X(80).
             10 PI-DRKI2114-HPVKUENDIGUNGKZ   PIC X(2).
             10 PI-DRKI2114-HPVLFREIBISDAT   PIC X(10).
             10 PI-DRKI2114-HPVLFREIVONDAT   PIC X(10).
             10 PI-DRKI2114-HPVMAILINGAKTION   PIC X(70).
             10 PI-DRKI2114-HPVMINDBEITRKZ   PIC X(2).
             10 PI-DRKI2114-HPVMITVERSPERSON   PIC X(35).
             10 PI-DRKI2114-HPVNAMELTERNVJKI   PIC X(45).
             10 PI-DRKI2114-HPVNAMVJKINDERN   PIC X(45).
             10 PI-DRKI2114-HPVPRODUKT   PIC X(30).
             10 PI-DRKI2114-HPVRANSCHRHAUSNR   PIC X(15).
             10 PI-DRKI2114-HPVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2114-HPVRANSCHRIFTORT   PIC X(30).
             10 PI-DRKI2114-HPVRANSCHRIFTPLZ   PIC X(6).
             10 PI-DRKI2114-HPVRANSCHRIFTSTR   PIC X(40).
             10 PI-DRKI2114-HPVREGULIERUNGKZ   PIC X(2).
             10 PI-DRKI2114-HPVRISIKO   PIC X(50).
             10 PI-DRKI2114-HPVSELBSTBFEST   PIC S9(9) COMP.
             10 PI-DRKI2114-HPVSELBSTBKZ   PIC X(2).
             10 PI-DRKI2114-HPVSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2114-HPVSELBSTBTEXT   PIC X(50).
             10 PI-DRKI2114-HPVUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2114-HPVVERSNUMMER   PIC X(40).
             10 PI-DRKI2114-HPVVERWSCHADKZ   PIC X(2).
             10 PI-DRKI2114-HPVZIELGRUPPE   PIC X(30).
             10 PI-DRKI2114-HPVZUSATZINFO   PIC X(100).
             
      *
       01  PO-DRKI2114.
           05  PO-DRKI2114-RC                 PIC  X(003).
               88 PO-DRKI2114-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2114-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
