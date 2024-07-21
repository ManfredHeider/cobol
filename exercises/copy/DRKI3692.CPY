      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3692.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3692.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 14:49:40   t13ex78
      *AKI01692: Standard-OZPs
      *
      *   Rev 2.0   Jun 24 2019 13:22:23 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3692                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3692                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.08.2019 / 08:14:28 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3692
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3692.
           05  PI-DRKI3692-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3692-DIRLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3692-DIRVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3692.
           05  PO-DRKI3692-RC                     PIC  X(003).
               88 PO-DRKI3692-RC-OK               VALUE 'OK '.
               88 PO-DRKI3692-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3692-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3692-DIRBUCHDAT   PIC  X(010).
             10 PO-DRKI3692-DIREMAIL.
                49 PO-DRKI3692-DIREMAIL-LEN PIC S9(4) COMP.
                49 PO-DRKI3692-DIREMAIL-TEXT PIC X(40).
             10 PO-DRKI3692-DIRGRAMM   PIC S9(10)V9(2) COMP-3.
             10 PO-DRKI3692-DIRMETALL   PIC  X(0001).
             10 PO-DRKI3692-DIRPRODBEZ.
                49 PO-DRKI3692-DIRPRODBEZ-LEN PIC S9(4) COMP.
                49 PO-DRKI3692-DIRPRODBEZ-TEXT PIC X(30).
             10 PO-DRKI3692-DIRPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI3692-DIRRABATT   PIC  X(0001).
             10 PO-DRKI3692-DIRSTUECKZAHL   PIC S9(4) COMP.
             10 PO-DRKI3692-DIRUMSATZ   PIC S9(7)V9(2) COMP-3.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

