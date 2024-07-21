      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2112.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2112.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:00:56   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 14:24:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2112                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2112                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.02.2008/18:57:07
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
      *    FÜR DIE FUNKTION DRKI2112
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2112.
           05  PI-DRKI2112-KEY.


             10 PI-DRKI2112-GLVVERTRAGSID   PIC X(13).
             10 PI-DRKI2112-GLVVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2112.
           05  PO-DRKI2112-RC                 PIC  X(003).
               88 PO-DRKI2112-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2112-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2112-DATEN.


             10 PO-DRKI2112-GLVBEDINGUNG   PIC X(16).
             10 PO-DRKI2112-GLVDIFDECKKZ   PIC X(2).
             10 PO-DRKI2112-GLVEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2112-GLVGEBAEUTYP-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2112-GLVGEBAEUTYP-KTY   PIC S9(4) COMP.
             10 PO-DRKI2112-GLVGESELLSCHAFT   PIC X(50).
             10 PO-DRKI2112-GLVKLAUSELTARIF   PIC X(80).
             10 PO-DRKI2112-GLVKUENDIGUNGKZ   PIC X(2).
             10 PO-DRKI2112-GLVLFREIBISDAT   PIC X(10).
             10 PO-DRKI2112-GLVLFREIVONDAT   PIC X(10).
             10 PO-DRKI2112-GLVMAILINGAKTION   PIC X(70).
             10 PO-DRKI2112-GLVMINBEITRAGKZ   PIC X(2).
             10 PO-DRKI2112-GLVRANSCHRHAUSNR   PIC X(15).
             10 PO-DRKI2112-GLVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PO-DRKI2112-GLVRANSCHRIFTORT   PIC X(30).
             10 PO-DRKI2112-GLVRANSCHRIFTPLZ   PIC X(6).
             10 PO-DRKI2112-GLVRANSCHRIFTSTR   PIC X(40).
             10 PO-DRKI2112-GLVRISIKO   PIC X(50).
             10 PO-DRKI2112-GLVSBFEST   PIC S9(9) COMP.
             10 PO-DRKI2112-GLVSBPROZENT   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI2112-GLVUMSTELLUNGDAT   PIC X(10).
             10 PO-DRKI2112-GLVVERSNUMMER   PIC X(40).
             10 PO-DRKI2112-GLVZUSATZINFO   PIC X(100).
             10 PO-DRKI2112-LETZTEAENDERUNG   PIC X(26).
             

      *
      ******* ENDE COPY-MEMBER **************************************
