      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2117.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2117.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:01:30   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 14:25:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2117                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2117                    *
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
      *    FÜR DIE FUNKTION DRKI2117
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2117.
           05  PI-DRKI2117-KEY.


             10 PI-DRKI2117-HPVVERTRAGSID   PIC X(13).
             10 PI-DRKI2117-HPVVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2117.
           05  PO-DRKI2117-RC                 PIC  X(003).
               88 PO-DRKI2117-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2117-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2117-DATEN.


             10 PO-DRKI2117-HPVABWEICHENDEVP   PIC X(35).
             10 PO-DRKI2117-HPVAUSLSCHADKZ   PIC X(2).
             10 PO-DRKI2117-HPVBEDINGUNG   PIC X(16).
             10 PO-DRKI2117-HPVDIFFDECKKZ   PIC X(2).
             10 PO-DRKI2117-HPVEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2117-HPVGESELLSCHAFT   PIC X(50).
             10 PO-DRKI2117-HPVKLAUSELTARIF   PIC X(80).
             10 PO-DRKI2117-HPVKUENDIGUNGKZ   PIC X(2).
             10 PO-DRKI2117-HPVLFREIBISDAT   PIC X(10).
             10 PO-DRKI2117-HPVLFREIVONDAT   PIC X(10).
             10 PO-DRKI2117-HPVMAILINGAKTION   PIC X(70).
             10 PO-DRKI2117-HPVMINDBEITRKZ   PIC X(2).
             10 PO-DRKI2117-HPVMITVERSPERSON   PIC X(35).
             10 PO-DRKI2117-HPVNAMELTERNVJKI   PIC X(45).
             10 PO-DRKI2117-HPVNAMVJKINDERN   PIC X(45).
             10 PO-DRKI2117-HPVPRODUKT   PIC X(30).
             10 PO-DRKI2117-HPVRANSCHRHAUSNR   PIC X(15).
             10 PO-DRKI2117-HPVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PO-DRKI2117-HPVRANSCHRIFTORT   PIC X(30).
             10 PO-DRKI2117-HPVRANSCHRIFTPLZ   PIC X(6).
             10 PO-DRKI2117-HPVRANSCHRIFTSTR   PIC X(40).
             10 PO-DRKI2117-HPVREGULIERUNGKZ   PIC X(2).
             10 PO-DRKI2117-HPVRISIKO   PIC X(50).
             10 PO-DRKI2117-HPVSELBSTBFEST   PIC S9(9) COMP.
             10 PO-DRKI2117-HPVSELBSTBKZ   PIC X(2).
             10 PO-DRKI2117-HPVSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2117-HPVSELBSTBTEXT   PIC X(50).
             10 PO-DRKI2117-HPVUMSTELLUNGDAT   PIC X(10).
             10 PO-DRKI2117-HPVVERSNUMMER   PIC X(40).
             10 PO-DRKI2117-HPVVERWSCHADKZ   PIC X(2).
             10 PO-DRKI2117-HPVZIELGRUPPE   PIC X(30).
             10 PO-DRKI2117-HPVZUSATZINFO   PIC X(100).
             10 PO-DRKI2117-LETZTEAENDERUNG   PIC X(26).
             

      *
      ******* ENDE COPY-MEMBER **************************************
