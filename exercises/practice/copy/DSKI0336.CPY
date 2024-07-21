      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0336.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DSKI0336.CPv  $
      *
      *   Rev 2.4   Mar 03 2020 15:19:36   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro) 
      *eilschicht
      *
      *   Rev 2.3   Aug 14 2018 09:35:20   t13ac17
      *AKI01605: G20 EVWS - neue Produkte zum RT 09/2018 Sourcen
      *
      *   Rev 2.2   Jun 23 2016 10:41:46   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.1   Feb 10 2011 16:57:10   t13ex27
      *AKI00907: Neue AMLeben-Tarife 2011 für R5.28-Copy
      *
      *   Rev 2.0   Feb 03 2011 15:13:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00907!03.02.11!2.1 ! NEU                       !I. Frenzel*
020516* AKI01434!02.05.16!2.2 ! Neue Produkte RT 06/16    !I. Frenzel*
040718* AKI01605!04.07.18!2.3 ! Neue Produkte RT 09/18    !Walbert   *
040718* AKI01757!03.03.20!2.4 ! Neue Produkte RT 03/20    !T13AC20   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Dieses Copy enthält die Werte der Tabelle TEILTYPLEBEN
      *
      ****************************************************************
      *
       01  K-TEILTYP                  PIC  9(002).
           88 K-TEILTYP-OK                VALUE
 
      *    Haupversicherung
                                                            01
      *    BUZV zur Beitragsbefreiung
                                                            11
      *    BUZV zur Barrente
                                                            12
      *    Unfall-Zusatzversicherung
                                                            13
      *    Witwen/r-Zusatzversicherung
                                                            14
      *    Erlebensfall-Zusatzversicherung
                                                            15
      *    EUZS Erwerbsunfähigkeitszusatzschutz
                                                            16
      *    Sterbegeld- und Sterberenten-Zusatzversicherung
                                                            18
      *    Kinder-(und Polio-) Zusatzversicherung
                                                            19
      *    Kinderinvaliditäts-Zusatzversicherung
                                                            20
      *    Sonderzahlung
                                                            21
      *    Todesfall-Zusatzversicherung
                                                            22
      *    Pflegerentenoption
                                                            23
      *    Pflegerenten-Zusatzversicherung
                                                            24
      *    EUZV zur Beitragsbefreiung
                                                            25
      *    EUZV einer Barrente
                                                            26
020516*    Schwere-Krankheiten Zusatzversicherung
020516                                                      27
040718*    BO-Option
040718                                                      28
040718*    Versorger-Todesfallzusatzversicherung
040718                                                      29
040718*    Versorger-Berufsunfähigkeitszusatzversicherung
040718                                                      30
      *    PZV zur Beitragsbefreiung
                                                            31.
 
       01  K-AML-HAUPT                PIC  9(02)  VALUE 1.
 


