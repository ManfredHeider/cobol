      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2110.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2110.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:00:40   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 14:24:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2110                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2110                    *
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
      *    FÜR DIE FUNKTION DRKI2110
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2110.
           05  PI-DRKI2110-KEY.


             10 PI-DRKI2110-GLVVERTRAGSID   PIC X(13).
             10 PI-DRKI2110-GLVVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2110-DATEN.


             10 PI-DRKI2110-GLVBEDINGUNG   PIC X(16).
             10 PI-DRKI2110-GLVDIFDECKKZ   PIC X(2).
             10 PI-DRKI2110-GLVEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2110-GLVGEBAEUTYP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2110-GLVGESELLSCHAFT   PIC X(50).
             10 PI-DRKI2110-GLVKLAUSELTARIF   PIC X(80).
             10 PI-DRKI2110-GLVKUENDIGUNGKZ   PIC X(2).
             10 PI-DRKI2110-GLVLFREIBISDAT   PIC X(10).
             10 PI-DRKI2110-GLVLFREIVONDAT   PIC X(10).
             10 PI-DRKI2110-GLVMAILINGAKTION   PIC X(70).
             10 PI-DRKI2110-GLVMINBEITRAGKZ   PIC X(2).
             10 PI-DRKI2110-GLVRANSCHRHAUSNR   PIC X(15).
             10 PI-DRKI2110-GLVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2110-GLVRANSCHRIFTORT   PIC X(30).
             10 PI-DRKI2110-GLVRANSCHRIFTPLZ   PIC X(6).
             10 PI-DRKI2110-GLVRANSCHRIFTSTR   PIC X(40).
             10 PI-DRKI2110-GLVRISIKO   PIC X(50).
             10 PI-DRKI2110-GLVSBFEST   PIC S9(9) COMP.
             10 PI-DRKI2110-GLVSBPROZENT   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2110-GLVUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2110-GLVVERSNUMMER   PIC X(40).
             10 PI-DRKI2110-GLVZUSATZINFO   PIC X(100).
             

      *
       01  PO-DRKI2110.
           05  PO-DRKI2110-RC                 PIC  X(003).
               88 PO-DRKI2110-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2110-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
