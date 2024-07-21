      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2109.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2109.CPv  $
      *
      *   Rev 2.1   Feb 27 2008 20:00:34   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 14:24:00   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2109                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2109                    *
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
      *    FÜR DIE FUNKTION DRKI2109
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2109.
           05  PI-DRKI2109-KEY.


             10 PI-DRKI2109-GLVVERTRAGSID   PIC X(13).
             10 PI-DRKI2109-GLVVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2109-DATEN.


             10 PI-DRKI2109-GLVBEDINGUNG   PIC X(16).
             10 PI-DRKI2109-GLVDIFDECKKZ   PIC X(2).
             10 PI-DRKI2109-GLVEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2109-GLVGEBAEUTYP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2109-GLVGESELLSCHAFT   PIC X(50).
             10 PI-DRKI2109-GLVKLAUSELTARIF   PIC X(80).
             10 PI-DRKI2109-GLVKUENDIGUNGKZ   PIC X(2).
             10 PI-DRKI2109-GLVLFREIBISDAT   PIC X(10).
             10 PI-DRKI2109-GLVLFREIVONDAT   PIC X(10).
             10 PI-DRKI2109-GLVMAILINGAKTION   PIC X(70).
             10 PI-DRKI2109-GLVMINBEITRAGKZ   PIC X(2).
             10 PI-DRKI2109-GLVRANSCHRHAUSNR   PIC X(15).
             10 PI-DRKI2109-GLVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2109-GLVRANSCHRIFTORT   PIC X(30).
             10 PI-DRKI2109-GLVRANSCHRIFTPLZ   PIC X(6).
             10 PI-DRKI2109-GLVRANSCHRIFTSTR   PIC X(40).
             10 PI-DRKI2109-GLVRISIKO   PIC X(50).
             10 PI-DRKI2109-GLVSBFEST   PIC S9(9) COMP.
             10 PI-DRKI2109-GLVSBPROZENT   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI2109-GLVUMSTELLUNGDAT   PIC X(10).
             10 PI-DRKI2109-GLVVERSNUMMER   PIC X(40).
             10 PI-DRKI2109-GLVZUSATZINFO   PIC X(100).
             
      *
       01  PO-DRKI2109.
           05  PO-DRKI2109-RC                 PIC  X(003).
               88 PO-DRKI2109-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2109-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
