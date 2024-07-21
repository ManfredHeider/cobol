      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2115.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2115.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:01:16   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 14:25:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2115                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2115                    *
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
      *    FÜR DIE FUNKTION DRKI2115
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2115.
           05  PI-DRKI2115-KEY.


             10 PI-DRKI2115-HPVVERTRAGSID   PIC X(13).
             10 PI-DRKI2115-HPVVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2115-DATEN.


             10 PI-DRKI2115-HPVABWEICHENDEVP   PIC X(35).
             10 PI-DRKI2115-HPVAUSLSCHADKZ   PIC X(2).
             10 PI-DRKI2115-HPVBEDINGUNG   PIC X(16).
             10 PI-DRKI2115-HPVDIFFDECKKZ   PIC X(2).
             10 PI-DRKI2115-HPVEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2115-HPVGESELLSCHAFT   PIC X(50).
             10 PI-DRKI2115-HPVKLAUSELTARIF   PIC X(80).
             10 PI-DRKI2115-HPVKUENDIGUNGKZ   PIC X(2).
             10 PI-DRKI2115-HPVLFREIBISDAT   PIC X(10).
             10 PI-DRKI2115-HPVLFREIVONDAT   PIC X(10).
             10 PI-DRKI2115-HPVMAILINGAKTION   PIC X(70).
             10 PI-DRKI2115-HPVMINDBEITRKZ   PIC X(2).
             10 PI-DRKI2115-HPVMITVERSPERSON   PIC X(35).
             10 PI-DRKI2115-HPVNAMELTERNVJKI   PIC X(45).
             10 PI-DRKI2115-HPVNAMVJKINDERN   PIC X(45).
             10 PI-DRKI2115-HPVPRODUKT   PIC X(30).
             10 PI-DRKI2115-HPVRANSCHRHAUSNR   PIC X(15).
             10 PI-DRKI2115-HPVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2115-HPVRANSCHRIFTORT   PIC X(30).
             10 PI-DRKI2115-HPVRANSCHRIFTPLZ   PIC X(6).
             10 PI-DRKI2115-HPVRANSCHRIFTSTR   PIC X(40).
             10 PI-DRKI2115-HPVREGULIERUNGKZ   PIC X(2).
             10 PI-DRKI2115-HPVRISIKO   PIC X(50).
             10 PI-DRKI2115-HPVSELBSTBFEST   PIC S9(9) COMP.
             10 PI-DRKI2115-HPVSELBSTBKZ   PIC X(2).
             10 PI-DRKI2115-HPVSELBSTBPROZ   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2115-HPVSELBSTBTEXT   PIC X(50).
             10 PI-DRKI2115-HPVUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2115-HPVVERSNUMMER   PIC X(40).
             10 PI-DRKI2115-HPVVERWSCHADKZ   PIC X(2).
             10 PI-DRKI2115-HPVZIELGRUPPE   PIC X(30).
             10 PI-DRKI2115-HPVZUSATZINFO   PIC X(100).
             

      *
       01  PO-DRKI2115.
           05  PO-DRKI2115-RC                 PIC  X(003).
               88 PO-DRKI2115-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2115-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
