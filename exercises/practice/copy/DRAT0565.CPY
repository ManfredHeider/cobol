      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0565.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0565.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:09:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  TABELLE-LAND.
           05 TABL-WERTE.
              10                       pic X(53) value
                'A            Österreich                              '.
              10                       pic X(53) value
                'AFG          Afghanistan                             '.
              10                       pic X(53) value
                'AGO          Angola                                  '.
              10                       pic X(53) value
                'AJ           Amerik.-Jungferninseln                  '.
              10                       pic X(53) value
                'AL           Albanien                                '.
              10                       pic X(53) value
                'AND          Andorra                                 '.
              10                       pic X(53) value
                'ANG          Anguilla                                '.
              10                       pic X(53) value
                'ANT          Antigua und Barbuda                     '.
              10                       pic X(53) value
                'AQU          Äquatorialguinea                        '.
              10                       pic X(53) value
                'ARM          Armenien                                '.
              10                       pic X(53) value
                'AS           Amerik.-Samoa                           '.
              10                       pic X(53) value
                'ASE          Aserbaidschan                           '.
              10                       pic X(53) value
                'AT           Antarktis-Territorium                   '.
              10                       pic X(53) value
                'AUS          Australien                              '.
              10                       pic X(53) value
                'B            Belgien                                 '.
              10                       pic X(53) value
                'BD           Bangladesch                             '.
              10                       pic X(53) value
                'BDS          Barbados                                '.
              10                       pic X(53) value
                'BER          Bermuda                                 '.
              10                       pic X(53) value
                'BG           Bulgarien                               '.
              10                       pic X(53) value
                'BH           Belize                                  '.
              10                       pic X(53) value
                'BHT          Bhutan                                  '.
              10                       pic X(53) value
                'BIH          Bosnien-Herzegowina                     '.
              10                       pic X(53) value
                'BIO          Malediven                               '.
              10                       pic X(53) value
                'BJ           Brit.-Jungferninseln                    '.
              10                       pic X(53) value
                'BOL          Bolivien                                '.
              10                       pic X(53) value
                'BR           Brasilien                               '.
              10                       pic X(53) value
                'BRN          Bahrain                                 '.
              10                       pic X(53) value
                'BRU          Brunei Darussalam                       '.
              10                       pic X(53) value
                'BS           Bahamas                                 '.
              10                       pic X(53) value
                'BY           Weißrußland                             '.
              10                       pic X(53) value
                'C            Kuba                                    '.
              10                       pic X(53) value
                'CAM          Kamerun                                 '.
              10                       pic X(53) value
                'CDN          Kanada                                  '.
              10                       pic X(53) value
                'CH           Schweiz                                 '.
              10                       pic X(53) value
                'CHD          Tschad                                  '.
              10                       pic X(53) value
                'CI           Cote d´Ivoire                           '.
              10                       pic X(53) value
                'CL           Sri Lanka                               '.
              10                       pic X(53) value
                'CO           Kolumbien                               '.
              10                       pic X(53) value
                'COI          Cookinseln                              '.
              10                       pic X(53) value
                'CR           Costa Rica                              '.
              10                       pic X(53) value
                'CUE          Canton und Enderbury                    '.
              10                       pic X(53) value
                'CV           Kap Verde                               '.
              10                       pic X(53) value
                'CY           Zypern                                  '.
              10                       pic X(53) value
                'CZ           Tschechische Republik                   '.
              10                       pic X(53) value
                'D  BRD       Bundesrepublik Deutschland              '.
              10                       pic X(53) value
                'DK           Dänemark                                '.
              10                       pic X(53) value
                'DOM          Dominikanische Republik                 '.
              10                       pic X(53) value
                'DSC          Dschibuti                               '.
              10                       pic X(53) value
                'DY           Benin                                   '.
              10                       pic X(53) value
                'DZ           Algerien                                '.
              10                       pic X(53) value
                'E            Spanien                                 '.
              10                       pic X(53) value
                'EAK          Kenia                                   '.
              10                       pic X(53) value
                'EAT          Tansania                                '.
              10                       pic X(53) value
                'EAU          Uganda                                  '.
              10                       pic X(53) value
                'EC           Ecuador                                 '.
              10                       pic X(53) value
                'ERI          Eritrea                                 '.
              10                       pic X(53) value
                'ES           El Salvador                             '.
              10                       pic X(53) value
                'EST          Estland                                 '.
              10                       pic X(53) value
                'ET           Ägypten                                 '.
              10                       pic X(53) value
                'ETH          Äthiopien                               '.
              10                       pic X(53) value
                'F            Frankreich                              '.
              10                       pic X(53) value
                'FAL          Falklandinseln                          '.
              10                       pic X(53) value
                'FG           Franz.-Guayana                          '.
              10                       pic X(53) value
                'FIN          Finnland                                '.
              10                       pic X(53) value
                'FJI          Fidschi                                 '.
              10                       pic X(53) value
                'FL           Liechtenstein                           '.
              10                       pic X(53) value
                'FP           Franz.-Polynesien                       '.
              10                       pic X(53) value
                'FR           Faröer                                  '.
              10                       pic X(53) value
                'GAB          Gabun                                   '.
              10                       pic X(53) value
                'GB           Großbritannien                          '.
              10                       pic X(53) value
                'GCA          Guatemala                               '.
              10                       pic X(53) value
                'GEO          Georgien                                '.
              10                       pic X(53) value
                'GH           Ghana                                   '.
              10                       pic X(53) value
                'GIB          Gibraltar                               '.
              10                       pic X(53) value
                'GR           Griechenland                            '.
              10                       pic X(53) value
                'GRO          Grönland                                '.
              10                       pic X(53) value
                'GUA          Guadeloupe                              '.
              10                       pic X(53) value
                'GUB          Guinea-Bissau                           '.
              10                       pic X(53) value
                'GUM          Guam                                    '.
              10                       pic X(53) value
                'GUY          Guyana                                  '.
              10                       pic X(53) value
                'H            Ungarn                                  '.
              10                       pic X(53) value
                'HCA          Honduras                                '.
              10                       pic X(53) value
                'HEL          St. Helena                              '.
              10                       pic X(53) value
                'HOK          Hongkong                                '.
              10                       pic X(53) value
                'HR           Kroatien                                '.
              10                       pic X(53) value
                'HV           Burkina Faso                            '.
              10                       pic X(53) value
                'I            Italien                                 '.
              10                       pic X(53) value
                'IL           Israel                                  '.
              10                       pic X(53) value
                'IND          Indien                                  '.
              10                       pic X(53) value
                'IR           Iran                                    '.
              10                       pic X(53) value
                'IRL          Irland                                  '.
              10                       pic X(53) value
                'IRQ          Irak                                    '.
              10                       pic X(53) value
                'IS           Island                                  '.
              10                       pic X(53) value
                'J            Japan                                   '.
              10                       pic X(53) value
                'JA           Jamaika                                 '.
              10                       pic X(53) value
                'JOR          Jordanien                               '.
              10                       pic X(53) value
                'K            Kambodscha                              '.
              10                       pic X(53) value
                'KAI          Kaimaninseln                            '.
              10                       pic X(53) value
                'KAN          Kanalinseln                             '.
              10                       pic X(53) value
                'KAS          Kasachstan                              '.
              10                       pic X(53) value
                'KIB          Kiribati                                '.
              10                       pic X(53) value
                'KIS          Kirgisistan                             '.
              10                       pic X(53) value
                'KOM          Komoren                                 '.
              10                       pic X(53) value
                'KOR          Korea, Dem. Volksrepublik               '.
              10                       pic X(53) value
                'KOS          Kosovo                                  '.
              10                       pic X(53) value
                'KWT          Kuwait                                  '.
              10                       pic X(53) value
                'L            Luxemburg                               '.
              10                       pic X(53) value
                'LAO          Laos                                    '.
              10                       pic X(53) value
                'LAR          Libyen                                  '.
              10                       pic X(53) value
                'LB           Liberia                                 '.
              10                       pic X(53) value
                'LS           Lesotho                                 '.
              10                       pic X(53) value
                'LT           Litauen                                 '.
              10                       pic X(53) value
                'LV           Lettland                                '.
              10                       pic X(53) value
                'M            Malta                                   '.
              10                       pic X(53) value
                'MA           Marokko                                 '.
              10                       pic X(53) value
                'MAC          Macau                                   '.
              10                       pic X(53) value
                'MAL          Malaysia                                '.
              10                       pic X(53) value
                'MAN          Insel Man                               '.
              10                       pic X(53) value
                'MAO          Oman                                    '.
              10                       pic X(53) value
                'MAR          Marshallinseln                          '.
              10                       pic X(53) value
                'MAT          Martinique                              '.
              10                       pic X(53) value
                'MAY          Mayotte                                 '.
              10                       pic X(53) value
                'MC           Monaco                                  '.
              10                       pic X(53) value
                'MD           Moldau                                  '.
              10                       pic X(53) value
                'MEX          Mexiko                                  '.
              10                       pic X(53) value
                'MIK          Mikronesien                             '.
              10                       pic X(53) value
                'MK           Mazedonien                              '.
              10                       pic X(53) value
                'MOL          Moldawien                               '.
              10                       pic X(53) value
                'MON          Mongolei                                '.
              10                       pic X(53) value
                'MOT          Montserrat                              '.
              10                       pic X(53) value
                'MOZ          Mosambik                                '.
              10                       pic X(53) value
                'MS           Mauritius                               '.
              10                       pic X(53) value
                'MW           Malawi                                  '.
              10                       pic X(53) value
                'MYA          Myanmar                                 '.
              10                       pic X(53) value
                'N            Norwegen                                '.
              10                       pic X(53) value
                'NAU          Nauru                                   '.
              10                       pic X(53) value
                'NEP          Nepal                                   '.
              10                       pic X(53) value
                'NIC          Nicaragua                               '.
              10                       pic X(53) value
                'NIU          Niue                                    '.
              10                       pic X(53) value
                'NKA          Neukaledonien                           '.
              10                       pic X(53) value
                'NL           Niederlande                             '.
              10                       pic X(53) value
                'NLA          Niederl. Antillen                       '.
              10                       pic X(53) value
                'NMA          Nördliche Marianen                      '.
              10                       pic X(53) value
                'NNG          Niederländisch-Neuguinea                '.
              10                       pic X(53) value
                'NZ           Neuseeland                              '.
              10                       pic X(53) value
                'O            Deutschland (Ost)                       '.
              10                       pic X(53) value
                'OTI          Timor-Leste                             '.
              10                       pic X(53) value
                'P            Portugal                                '.
              10                       pic X(53) value
                'PA           Panama                                  '.
              10                       pic X(53) value
                'PAK          Pakistan                                '.
              10                       pic X(53) value
                'PAL          Palau                                   '.
              10                       pic X(53) value
                'PE           Peru                                    '.
              10                       pic X(53) value
                'PIE          Saint Pierre und Miquelon               '.
              10                       pic X(53) value
                'PIN          Pazifische Inseln                       '.
              10                       pic X(53) value
                'PIT          Pitcairn-Insel                          '.
              10                       pic X(53) value
                'PK           Pakistan                                '.
              10                       pic X(53) value
                'PL           Polen                                   '.
              10                       pic X(53) value
                'PNG          Papua-Neuguinea                         '.
              10                       pic X(53) value
                'PRI          Puerto Rico                             '.
              10                       pic X(53) value
                'PY           Paraguay                                '.
              10                       pic X(53) value
                'QAT          Katar                                   '.
              10                       pic X(53) value
                'RA           Argentinien                             '.
              10                       pic X(53) value
                'RB           Botsuana                                '.
              10                       pic X(53) value
                'RC           Taiwan                                  '.
              10                       pic X(53) value
                'RCA          Zentralafrikanische Republik            '.
              10                       pic X(53) value
                'RCB          Kongo                                   '.
              10                       pic X(53) value
                'RCH          Chile                                   '.
              10                       pic X(53) value
                'REU          Reunion                                 '.
              10                       pic X(53) value
                'RG           Guinea                                  '.
              10                       pic X(53) value
                'RH           Haiti                                   '.
              10                       pic X(53) value
                'RI           Indonesien                              '.
              10                       pic X(53) value
                'RIM          Mauretanien                             '.
              10                       pic X(53) value
                'RL           Libanon                                 '.
              10                       pic X(53) value
                'RM           Madagaskar                              '.
              10                       pic X(53) value
                'RMM          Mali                                    '.
              10                       pic X(53) value
                'RN           Niger                                   '.
              10                       pic X(53) value
                'RO           Rumänien                                '.
              10                       pic X(53) value
                'ROC          Taiwan (politisch China)                '.
              10                       pic X(53) value
                'ROK          Korea, Republik                         '.
              10                       pic X(53) value
                'ROU          Uruguay                                 '.
              10                       pic X(53) value
                'RP           Philippinen                             '.
              10                       pic X(53) value
                'RSM          San Marino                              '.
              10                       pic X(53) value
                'RU           Burundi                                 '.
              10                       pic X(53) value
                'RUS          Russ. Föderation                        '.
              10                       pic X(53) value
                'RWA          Ruanda                                  '.
              10                       pic X(53) value
                'S            Schweden                                '.
              10                       pic X(53) value
                'SAU          Saudi-Arabien                           '.
              10                       pic X(53) value
                'SCG          Serbien-Montenegro                      '.
              10                       pic X(53) value
                'SCN          St. Kitts und Nevis                     '.
              10                       pic X(53) value
                'SD           Swasiland                               '.
              10                       pic X(53) value
                'SF           Finnland                                '.
              10                       pic X(53) value
                'SGP          Singapur                                '.
              10                       pic X(53) value
                'SK           Slowakei                                '.
              10                       pic X(53) value
                'SLO          Slowenien                               '.
              10                       pic X(53) value
                'SME          Suriname                                '.
              10                       pic X(53) value
                'SN           Senegal                                 '.
              10                       pic X(53) value
                'SOL          Salomonen                               '.
              10                       pic X(53) value
                'SP           Somalia                                 '.
              10                       pic X(53) value
                'SRB          Serbien                                 '.
              10                       pic X(53) value
                'STL          Staatenlos                              '.
              10                       pic X(53) value
                'STP          Sao Tome und Principe                   '.
              10                       pic X(53) value
                'SUD          Sudan                                   '.
              10                       pic X(53) value
                'SWA          Namibia                                 '.
              10                       pic X(53) value
                'SY           Seychellen                              '.
              10                       pic X(53) value
                'SYR          Syrien                                  '.
              10                       pic X(53) value
                'T            Thailand                                '.
              10                       pic X(53) value
                'TAD          Tadschikistan                           '.
              10                       pic X(53) value
                'TG           Togo                                    '.
              10                       pic X(53) value
                'TJ           Volksrepublik China                     '.
              10                       pic X(53) value
                'TN           Tunesien                                '.
              10                       pic X(53) value
                'TOK          Tokelau-Inseln                          '.
              10                       pic X(53) value
                'TON          Tonga                                   '.
              10                       pic X(53) value
                'TR           Türkei                                  '.
              10                       pic X(53) value
                'TT           Trinidad und Tobago                     '.
              10                       pic X(53) value
                'TUC          Turks- und Caicosinseln                 '.
              10                       pic X(53) value
                'TUR          Turkmenistan                            '.
              10                       pic X(53) value
                'TUV          Tuvalu                                  '.
              10                       pic X(53) value
                'UA           Ukraine                                 '.
              10                       pic X(53) value
                'UAE          Vereinigte Arabische Emirate            '.
              10                       pic X(53) value
                'UNB          Unbekannt                               '.
              10                       pic X(53) value
                'USAUSA       Vereinigte Staaten von Amerika          '.
              10                       pic X(53) value
                'USB          Usbekistan                              '.
              10                       pic X(53) value
                'V            Vatikanstadt                            '.
              10                       pic X(53) value
                'VAN          Vanuatu                                 '.
              10                       pic X(53) value
                'VN           Vietnam                                 '.
              10                       pic X(53) value
                'WAG          Gambia                                  '.
              10                       pic X(53) value
                'WAL          Sierra Leone                            '.
              10                       pic X(53) value
                'WAN          Nigeria                                 '.
              10                       pic X(53) value
                'WD           Dominica                                '.
              10                       pic X(53) value
                'WG           Grenada                                 '.
              10                       pic X(53) value
                'WL           St. Lucia                               '.
              10                       pic X(53) value
                'WS           Samoa                                   '.
              10                       pic X(53) value
                'WV           St. Vincent und die Grenadinen          '.
              10                       pic X(53) value
                'XXX          falsche Eingabe                         '.
              10                       pic X(53) value
                'YEM          Jemen                                   '.
              10                       pic X(53) value
                'YU           Jugoslawien                             '.
              10                       pic X(53) value
                'YV           Venezuela                               '.
              10                       pic X(53) value
                'Z            Sambia                                  '.
              10                       pic X(53) value
                'ZA           Südafrika                               '.
              10                       pic X(53) value
                'ZRE          Kongo, Dem. Republik                    '.
              10                       pic X(53) value
                'ZW           Simbabwe                                '.
           05 redefines TABL-WERTE.
              10 TABL                  occurs 241
                                       indexed by TABL-IND.
                 15 TABL-LKZ           pic X(3).
                 15 TABL-LAND-KURZ     pic X(10).
                 15 TABL-LAND-LANG     pic X(40).
