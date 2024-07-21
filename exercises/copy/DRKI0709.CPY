      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0709.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0709.CPv  $
      *
      *   Rev 2.5   Jan 22 2007 13:43:06   t13ex27
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB Adressen
      *
      *   Rev 2.4   Jun 14 2005 08:51:22   t13ex49
      *AKI00481: Ermittl. mitversich. Pers. Krankenvers. die nicht 
      *Kunde sind
      *
      *   Rev 2.3   Feb 02 2004 07:53:28   t13ex49
      *AKI00268 - Übergabe post. Prüfung - Komplettlauf
      *
      *   Rev 2.2   Oct 13 2003 11:46:48   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.1   Jan 20 2003 09:45:14   t13ex27
      *AKI00303 - Kommissarische Kundenübertragung
      *
      *   Rev 2.0   Feb 27 1999 12:58:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 18:24:02   T13EX47
      *Funktionsauswahl LIST-OZP ZXKI0044
      *
      *   Rev 1.0   06 Oct 1998 15:31:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0709                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0044                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    13.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKIB0001!13.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      * AKI00303!   01.03!    ! Erweiterung DRKI1782      !Frenzel   *
210302* AKI00114!21.03.02!    ! Erweiterung DRKI1374      !Peter Pohl*
210302*         !        !    !         und DRKI1375      !          *
210302*         !        !    !         und DRKI1397      !          *
210302*         !        !    !         und DRKI1572      !          *
      * -------------------------------------------------------------*
171203* AKI00268!17.12.03!    !neu DRKI1627               !Peter Pohl*
      * -------------------------------------------------------------*
180405* AKI00481!18.04.05!    !neu DRKI1782               !Peter Pohl*
      * -------------------------------------------------------------*
      * AKI00556!28.08.06!    !neu DRKI1940               !I. Frenzel*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZXKI0044.
           05  PI-ZXKI0044-FUNKTION            PIC  X(008).
               88 PI-ZXKI0044-DRKI0710              VALUE 'DRKI0710'.
               88 PI-ZXKI0044-DRKI1481              VALUE 'DRKI1481'.
210302         88 PI-ZXKI0044-DRKI1374              VALUE 'DRKI1374'.
210302         88 PI-ZXKI0044-DRKI1375              VALUE 'DRKI1375'.
210302         88 PI-ZXKI0044-DRKI1397              VALUE 'DRKI1397'.
210302         88 PI-ZXKI0044-DRKI1572              VALUE 'DRKI1572'.
171203         88 PI-ZXKI0044-DRKI1627              VALUE 'DRKI1627'.
180405         88 PI-ZXKI0044-DRKI1782              VALUE 'DRKI1782'.
280806         88 PI-ZXKI0044-DRKI1940              VALUE 'DRKI1940'.
      *
      ******* ENDE COPY-MEMBER DRKI0709 *****************************
