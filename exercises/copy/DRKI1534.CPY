      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1534.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1534.CPv  $
      *
      *   Rev 2.6   Nov 24 2020 09:05:38   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.5   Feb 24 2020 11:43:44   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.4   Nov 06 2015 12:29:22   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens  OZP ZTKI0315
      *
      *   Rev 2.3   Apr 27 2005 14:26:48   t13ex39
      *AKI00249: ART DES DEPOTS
      *
      *   Rev 2.2   Jul 13 2003 09:49:52   T13EX39
      *AKI00327.UE005 Db-PRODUKT
      *
      *   Rev 2.0   Jan 13 2003 15:51:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1534                                     *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1534                   *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00327!19.03.03!01.0! Neu                       !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00249!18.03.05!01.1! PO-DRKI1534-GHPDEPOTART   !WHO       *
      * -------------------------------------------------------------*
061115* AKI01385!06.11.15!02.4! PROVISIONS KZ einbauen    !DDY       *
      *---------!--------!--------------------------------!----------*
      * AKI01762!19.01.20! NEUES ATTRIBUT "GHPBLZKUNDEN"  ! T13EX39
      *         !        ! UND BPKBLZKUNDEN AUS KUNDENBLZ !
      *         !        ! VERARBEITEN                    !
      *---------!--------!--------------------------------!----------*
      * AKI01805!09.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1534
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI1534.
          05 PI-DRKI1534-PROGRAMM               PIC X(008).
          05 PI-DRKI1534-UFKT                   PIC 9(002).
          05 PI-DRKI1534-DATEN.
             10 PI-DRKI1534-GHPVERTRAGSID       PIC X(13).
 
      *
       01 PO-DRKI1534.
 
          05 PO-DRKI1534-RC                     PIC  X(003).
             88 PO-DRKI1534-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1534-RC-EOF                   VALUE 'EOF'.
 
          05  PO-DRKI1534-KEY.
 
             10 PO-DRKI1534-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PO-DRKI1534-GHPVERTRAGSID        PIC X(13).
 
          05  PO-DRKI1534-DATEN.
             10 PO-DRKI1534-GHPABSCHLUSSVBNR      PIC S9(9) COMP.
             10 PO-DRKI1534-GHPANLAGENAME.
                15 PO-DRKI1534-GHPANLAGENAME-LEN   PIC S9(4) COMP.
                15 PO-DRKI1534-GHPANLAGENAME-TEXT   PIC X(15).
 
             10 PO-DRKI1534-GHPANLAGEZIEL   PIC X(01).
             10 PO-DRKI1534-GHPDEPOTART   PIC X(1).
             10 PO-DRKI1534-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1534-GHPEIGENVERMITTELT   PIC X(01).
             10 PO-DRKI1534-GHPFAXREVERS   PIC X(01).
             10 PO-DRKI1534-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1534-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PO-DRKI1534-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1534-GHPGUTHABENWKZ   PIC X(01).
             10 PO-DRKI1534-GHPGUTHWERTWKZ   PIC X(01).
             10 PO-DRKI1534-GHPINFOSPERREDATUM   PIC X(10).
             10 PO-DRKI1534-GHPINFOWPERREKZ   PIC X(01).
             10 PO-DRKI1534-GHPOFFNUNGSDATUM   PIC X(10).
             10 PO-DRKI1534-GHPONLINEREVERS   PIC X(01).
             10 PO-DRKI1534-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI1534-GHPPERFLFDJAHR  PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI1534-GHPPRODUKTID   PIC S9(4) COMP.
061115       10 PO-DRKI1534-GHPPROVISIONKZ   PIC  X(0001).
             10 PO-DRKI1534-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PO-DRKI1534-NI-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PO-DRKI1534-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1534-GHPSCHLAUSZGWKZ   PIC X(01).
             10 PO-DRKI1534-GHPSCHLIESSDATUM   PIC X(10).
             10 PO-DRKI1534-GHPSUBTYPE   PIC X(01).
             10 PO-DRKI1534-GHPTELEFONREVERS   PIC X(01).
             10 PO-DRKI1534-GHPVERFUEGUNGID   PIC X(2).
             10 PO-DRKI1534-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1534-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1534-GHPZWISCHENGEWWKZ   PIC X(01).
             10 PO-DRKI1534-GHPBLZKUNDEN        PIC X(10).
             10 PO-DRKI1534-GHPESAFE            PIC X(1).
      *
      ******* ENDE COPY-MEMBER **************************************



